! { dg-do run }
! { dg-require-effective-target fortran_large_real }

module testmod
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
contains
  subroutine testoutput (a,b,length,f)
    real(kind=k),intent(in) :: a
    real(kind=8),intent(in) ::  b
    integer,intent(in) :: length
    character(len=*),intent(in) :: f

    character(len=length) :: ca
    character(len=length) :: cb

    write (ca,f) a
    write (cb,f) b
    if (ca /= cb) call abort
  end subroutine testoutput

  subroutine outputstring (a,f,s)
    real(kind=k),intent(in) :: a
    character(len=*),intent(in) :: f
    character(len=*),intent(in) :: s

    character(len=len(s)) :: c
    
    write (c,f) a
    if (c /= s) call abort
  end subroutine outputstring
end module testmod


! Testing I/O of large real kinds (larger than kind=8)
program test
  use testmod
  implicit none

  real(kind=k) :: x
  character(len=20) :: c1, c2

  call testoutput (0.0_k,0.0_8,40,'(F40.35)')

  call testoutput (1.0_k,1.0_8,40,'(F40.35)')
  call testoutput (0.1_k,0.1_8,15,'(F15.10)')
  call testoutput (1e10_k,1e10_8,15,'(F15.10)')
  call testoutput (7.51e100_k,7.51e100_8,15,'(F15.10)')
  call testoutput (1e-10_k,1e-10_8,15,'(F15.10)')
  call testoutput (7.51e-100_k,7.51e-100_8,15,'(F15.10)')

  call testoutput (-1.0_k,-1.0_8,40,'(F40.35)')
  call testoutput (-0.1_k,-0.1_8,15,'(F15.10)')
  call testoutput (-1e10_k,-1e10_8,15,'(F15.10)')
  call testoutput (-7.51e100_k,-7.51e100_8,15,'(F15.10)')
  call testoutput (-1e-10_k,-1e-10_8,15,'(F15.10)')
  call testoutput (-7.51e-100_k,-7.51e-100_8,15,'(F15.10)')

  x = huge(x)
  call outputstring (2*x,'(F20.15)','           +Infinity')
  call outputstring (-2*x,'(F20.15)','           -Infinity')

  write (c1,'(G20.10E5)') x
  write (c2,'(G20.10E5)') -x
  if (c2(1:1) /= '-') call abort
  c2(1:1) = ' '
  if (c1 /= c2) call abort

  x = tiny(x)
  call outputstring (x,'(F20.15)','   0.000000000000000')
  call outputstring (-x,'(F20.15)','   0.000000000000000')

  write (c1,'(G20.10E5)') x
  write (c2,'(G20.10E5)') -x
  if (c2(1:1) /= '-') call abort
  c2(1:1) = ' '
  if (c1 /= c2) call abort
end program test

! { dg-final { cleanup-modules "testmod" } }
