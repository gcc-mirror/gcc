! { dg-do run }
! { dg-options "-std=gnu" }
! Test character variables in data statements
! Also substrings of character variables.
! PR14976 PR16228 
program data_char_1
  character(len=5) :: a(2)
  character(len=5) :: b(2)
  data a /'Hellow', 'orld'/       ! { dg-warning "truncated" }
  data b(:)(1:4), b(1)(5:5), b(2)(5:5) &
      /'abcdefg', 'hi', 'j', 'k'/ ! { dg-warning "truncated" }
  
  if ((a(1) .ne. 'Hello') .or. (a(2) .ne. 'orld ')) STOP 1
  if ((b(1) .ne. 'abcdj') .or. (b(2) .ne. 'hi  k')) STOP 2
end program
