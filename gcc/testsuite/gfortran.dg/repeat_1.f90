! { dg-do run }
! { dg-shouldfail "negative NCOPIES argument to REPEAT intrinsic" }
  character(len=80) :: str
  integer :: i
  i = -1
  write(str,"(a)") repeat ("a", f())
  if (trim(str) /= "aaaa") call abort
  write(str,"(a)") repeat ("a", i)

contains

  integer function f()
    integer :: x = 5
    save x

    x = x - 1
    f = x
  end function f
end
! { dg-output "Fortran runtime error: Argument NCOPIES of REPEAT intrinsic is negative \\(its value is -1\\)" }
