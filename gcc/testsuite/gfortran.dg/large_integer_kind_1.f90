! { dg-do run }
! { dg-require-effective-target fortran_large_int }

module testmod
  integer,parameter :: k = selected_int_kind (range (0_8) + 1)
contains
  subroutine testoutput (a,b,length,f)
    integer(kind=k),intent(in) :: a
    integer(kind=8),intent(in) ::  b
    integer,intent(in) :: length
    character(len=*),intent(in) :: f

    character(len=length) :: ca
    character(len=length) :: cb

    write (ca,f) a
    write (cb,f) b
    if (ca /= cb) STOP 1
  end subroutine testoutput
end module testmod


! Testing I/O of large integer kinds (larger than kind=8)
program test
  use testmod
  implicit none

  integer(kind=k) :: x
  character(len=50) :: c1, c2

  call testoutput (0_k,0_8,50,'(I50)')
  call testoutput (1_k,1_8,50,'(I50)')
  call testoutput (-1_k,-1_8,50,'(I50)')
  x = huge(0_8)
  call testoutput (x,huge(0_8),50,'(I50)')
  x = -huge(0_8)
  call testoutput (x,-huge(0_8),50,'(I50)')
end program test
