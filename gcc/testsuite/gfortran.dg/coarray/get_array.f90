! { dg-do run }
!
! This program does a correctness check for
! ... = ARRAY[idx] and ... = SCALAR[idx]
!


!
! FIXME: two/three has to be modified, test has to be checked and
! diagnostic has to be removed
! 

program main
  implicit none
  integer, parameter :: n = 3
  integer, parameter :: m = 4

  ! Allocatable coarrays
  call one(-5, 1)
  call one(0, 0)
  call one(1, -5)
  call one(0, -11)

  ! Static coarrays
  call two()
  call three()
contains
  subroutine one(lb1, lb2)
    integer, value :: lb1, lb2

    integer :: i_sgn1, i_sgn2, i, i_e, i_s, j, j_e, j_s
    integer, allocatable :: caf(:,:)[:]
    integer, allocatable :: a(:,:), b(:,:), c(:,:)

    allocate(caf(lb1:n+lb1-1, lb2:m+lb2-1)[*], &
         a(lb1:n+lb1-1, lb2:m+lb2-1), &
         b(lb1:n+lb1-1, lb2:m+lb2-1), &
         c(lb1:n+lb1-1, lb2:m+lb2-1))

    b = reshape([(i*33, i = 1, size(b))], shape(b))

    ! Whole array: ARRAY = ARRAY
    caf = -42
    a = -42
    c = -42
    if (this_image() == num_images()) then
      caf(:,:) = b(:,:)
    endif
    sync all
    a(:,:) = b(:,:)
    c(:,:) = caf(:,:)[num_images()]
    if (any (a /= c)) then
      STOP 1
    end if
    sync all

    ! Scalar assignment
    caf = -42
    a = -42
    c = -42
    if (this_image() == num_images()) then
      caf(:,:) = b(:,:)
    endif
    sync all
    do j = lb2, m+lb2-1
      do i = n+lb1-1, lb1, -2
        a(i,j) = b(i,j)
        c(i,j) = caf(i,j)[num_images()]
      end do
    end do
    do j = lb2, m+lb2-1
      do i = lb1, n+lb1-1, 2
        a(i,j) = b(i,j)
        c(i,j) = caf(i,j)[num_images()]
      end do
    end do
    if (any (a /= c)) then
      STOP 2
    end if
    sync all

    ! Array sections with different ranges and pos/neg strides
    do i_sgn1 = -1, 1, 2
      do i_sgn2 = -1, 1, 2
        do i=lb1, n+lb1-1
          do i_e=lb1, n+lb1-1
            do i_s=1, n
              do j=lb2, m+lb2-1
                do j_e=lb2, m+lb2-1
                  do j_s=1, m
                    ! ARRAY = ARRAY
                    caf = -42
                    a = -42
                    c = -42
                    if (this_image() == num_images()) then
                      caf(:,:) = b(:,:)
                    endif
                    sync all
                    a(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2) &
                         = b(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2)
                    c(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2) &
                         = caf(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2)[num_images()]
                    if (any (c /= a)) then
                      STOP 3
                    end if
                    sync all
                  end do
                end do
              end do
            end do
          end do
        end do
      end do
    end do
  end subroutine one

  subroutine two()
    integer, parameter :: lb1 = -5, lb2 = 1

    integer :: i_sgn1, i_sgn2, i, i_e, i_s, j, j_e, j_s
    integer, save :: caf(lb1:n+lb1-1, lb2:m+lb2-1)[*]
    integer, save :: a(lb1:n+lb1-1, lb2:m+lb2-1)
    integer, save :: b(lb1:n+lb1-1, lb2:m+lb2-1)
    integer, save :: c(lb1:n+lb1-1, lb2:m+lb2-1)

    b = reshape([(i*33, i = 1, size(b))], shape(b))

    ! Whole array: ARRAY = ARRAY
    caf = -42
    a = -42
    c = -42
    if (this_image() == num_images()) then
      caf(:,:) = b(:,:)
    endif
    sync all
    a(:,:) = b(:,:)
    c(:,:) = caf(:,:)[num_images()]
    if (any (a /= c)) then
      STOP 4
    end if
    sync all

    ! Scalar assignment
    caf = -42
    a = -42
    c = -42
    if (this_image() == num_images()) then
      caf(:,:) = b(:,:)
    endif
    sync all
    do j = lb2, m+lb2-1
      do i = n+lb1-1, lb1, -2
        a(i,j) = b(i,j)
        c(i,j) = caf(i,j)[num_images()]
      end do
    end do
    do j = lb2, m+lb2-1
      do i = lb1, n+lb1-1, 2
        a(i,j) = b(i,j)
        c(i,j) = caf(i,j)[num_images()]
      end do
    end do
    if (any (a /= c)) then
      STOP 5
    end if
    sync all

    ! Array sections with different ranges and pos/neg strides
    do i_sgn1 = -1, 1, 2
      do i_sgn2 = -1, 1, 2
        do i=lb1, n+lb1-1
          do i_e=lb1, n+lb1-1
            do i_s=1, n
              do j=lb2, m+lb2-1
                do j_e=lb2, m+lb2-1
                  do j_s=1, m
                    ! ARRAY = ARRAY
                    caf = -42
                    a = -42
                    c = -42
                    if (this_image() == num_images()) then
                      caf(:,:) = b(:,:)
                    endif
                    sync all
                    a(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2) &
                         = b(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2)
                    c(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2) &
                         = caf(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2)[num_images()]
                    if (any (c /= a)) then
                      STOP 6
                    end if
                    sync all
                  end do
                end do
              end do
            end do
          end do
        end do
      end do
    end do
  end subroutine two

  subroutine three()
    integer, parameter :: lb1 = 0, lb2 = 0

    integer :: i_sgn1, i_sgn2, i, i_e, i_s, j, j_e, j_s
    integer, save :: caf(lb1:n+lb1-1, lb2:m+lb2-1)[*]
    integer, save :: a(lb1:n+lb1-1, lb2:m+lb2-1)
    integer, save :: b(lb1:n+lb1-1, lb2:m+lb2-1)
    integer, save :: c(lb1:n+lb1-1, lb2:m+lb2-1)

    b = reshape([(i*33, i = 1, size(b))], shape(b))

    ! Whole array: ARRAY = ARRAY
    caf = -42
    a = -42
    c = -42
    if (this_image() == num_images()) then
      caf(:,:) = b(:,:)
    endif
    sync all
    a(:,:) = b(:,:)
    c(:,:) = caf(:,:)[num_images()]
    if (any (a /= c)) then
      STOP 7
    end if
    sync all

    ! Scalar assignment
    caf = -42
    a = -42
    c = -42
    if (this_image() == num_images()) then
      caf(:,:) = b(:,:)
    endif
    sync all
    do j = lb2, m+lb2-1
      do i = n+lb1-1, lb1, -2
        a(i,j) = b(i,j)
        c(i,j) = caf(i,j)[num_images()]
      end do
    end do
    do j = lb2, m+lb2-1
      do i = lb1, n+lb1-1, 2
        a(i,j) = b(i,j)
        c(i,j) = caf(i,j)[num_images()]
      end do
    end do
    if (any (a /= c)) then
      STOP 8
    end if
    sync all

    ! Array sections with different ranges and pos/neg strides
    do i_sgn1 = -1, 1, 2
      do i_sgn2 = -1, 1, 2
        do i=lb1, n+lb1-1
          do i_e=lb1, n+lb1-1
            do i_s=1, n
              do j=lb2, m+lb2-1
                do j_e=lb2, m+lb2-1
                  do j_s=1, m
                    ! ARRAY = ARRAY
                    caf = -42
                    a = -42
                    c = -42
                    if (this_image() == num_images()) then
                      caf(:,:) = b(:,:)
                    endif
                    sync all
                    a(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2) &
                         = b(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2)
                    c(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2) &
                         = caf(i:i_e:i_s*i_sgn1, j:j_e:j_s*i_sgn2)[num_images()]
                    if (any (c /= a)) then
                      STOP 9
                    end if
                    sync all
                  end do
                end do
              end do
            end do
          end do
        end do
      end do
    end do
  end subroutine three
end program main
