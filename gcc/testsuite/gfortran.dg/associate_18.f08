! { dg-do run }
!
! Contributed by Antony Lewis  <antony@cosmologist.info>
!                Andre Vehreschild  <vehre@gcc.gnu.org>
! Check that associating array-sections/scalars is working
! with class arrays.
!

program associate_18
  Type T
    integer :: map = 1
  end Type T

  class(T), allocatable :: av(:)
  class(T), allocatable :: am(:,:)
  class(T), pointer :: pv(:)
  class(T), pointer :: pm(:,:)

  integer :: iv(5) = 17
  integer :: im(4,5) = 23
  integer :: expect(20) = 23
  integer :: c

  allocate(av(2))
  associate(i => av(1))
    i%map = 2
  end associate
  if (any (av%map /= [2,1])) call abort()
  deallocate(av)

  allocate(am(3,4))
  associate(pam => am(2:3, 2:3))
    pam%map = 7
    pam(1,2)%map = 8
  end associate
  if (any (reshape(am%map, [12]) /= [1,1,1, 1,7,7, 1,8,7, 1,1,1])) call abort()
  deallocate(am)

  allocate(pv(2))
  associate(i => pv(1))
    i%map = 2
  end associate
  if (any (pv%map /= [2,1])) call abort()
  deallocate(pv)

  allocate(pm(3,4))
  associate(ppm => pm(2:3, 2:3))
    ppm%map = 7
    ppm(1,2)%map = 8
  end associate
  if (any (reshape(pm%map, [12]) /= [1,1,1, 1,7,7, 1,8,7, 1,1,1])) call abort()
  deallocate(pm)

  associate(i => iv(1))
    i = 7
  end associate
  if (any (iv /= [7, 17, 17, 17, 17])) call abort()

  associate(pam => im(2:3, 2:3))
    pam = 9
    pam(1,2) = 10
    do c = 1, 2
        pam(2, c) = 0
    end do
  end associate
  if (any (reshape(im, [20]) /= [23,23,23,23, 23,9,0,23, &
        23,10,0,23, 23,23,23,23, 23,23,23,23])) call abort()

  expect(2:3) = 9
  do c = 1, 5
    im = 23
    associate(pam => im(:, c))
      pam(2:3) = 9
    end associate
    if (any (reshape(im, [20]) /= expect)) call abort()
    ! Shift expect
    expect = [expect(17:), expect(:16)]
  end do
end program

