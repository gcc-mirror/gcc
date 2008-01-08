! { dg-do run }
! Checks the fixes for PR34681 and PR34704, in which various mixtures
! of default initializer and allocatable array were not being handled
! correctly for derived types with allocatable components.
!
! Contributed by Paolo Giannozzi <p.giannozzi@fisica.uniud.it>
!
program boh
  integer :: c1, c2, c3, c4, c5
  !
  call mah (0, c1) ! These calls deal with PR34681
  call mah (1, c2)
  call mah (2, c3)
  !
  if (c1 /= c2) call abort
  if (c1 /= c3) call abort
  !
  call mah0 (c4) ! These calls deal with PR34704
  call mah1 (c5)
  !
  if (c4 /= c5) call abort
  !
end program boh
!
subroutine mah (i, c)
  !
  integer, intent(in) :: i
  integer, intent(OUT) :: c
  !
  type mix_type
     real(8), allocatable :: a(:)
     complex(8), allocatable :: b(:)
  end type mix_type
  type(mix_type), allocatable, save :: t(:)
  integer :: j, n=1024
  !
  if (i==0) then
     allocate (t(1))
     allocate (t(1)%a(n))
     allocate (t(1)%b(n))
     do j=1,n
        t(1)%a(j) = j
        t(1)%b(j) = n-j
     end do
  end if
  c = sum( t(1)%a(:) ) + sum( t(1)%b(:) )
  if ( i==2) then
     deallocate (t(1)%b)
     deallocate (t(1)%a)
     deallocate (t)
  end if
end subroutine mah

subroutine mah0 (c)
  !
  integer, intent(OUT) :: c
  type mix_type
     real(8), allocatable :: a(:)
     integer :: n=1023
  end type mix_type
  type(mix_type) :: t
  !
  allocate(t%a(1))
  t%a=3.1415926
  c = t%n
  deallocate(t%a)
  !
end subroutine mah0
!
subroutine mah1 (c)
  !
  integer, intent(OUT) :: c
  type mix_type
     real(8), allocatable :: a(:)
     integer :: n=1023
  end type mix_type
  type(mix_type), save :: t
  !
  allocate(t%a(1))
  t%a=3.1415926
  c = t%n
  deallocate(t%a)
  !
end subroutine mah1
