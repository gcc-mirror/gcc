! { dg-do run }
! Testcase for PR fortran/25681
program char_type_len
  integer,parameter :: n = 9
  type foo_t
     character (len = 80) :: bar (1)
     character (len = 75) :: gee (n)
  end type foo_t
  type(foo_t) :: foo
  
  if (len(foo%bar) /= 80 .or. len(foo%gee) /= 75) STOP 1
end program char_type_len
