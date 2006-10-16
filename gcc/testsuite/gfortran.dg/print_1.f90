! { dg-do compile }
! PR fortran/29403
program p
   character(len=10) a, b, c
   integer i
   i = 1
   print ('(I0)'), i
   a = '(I0,'
   b = 'I2,'
   c = 'I4)'
   call prn(a, b, c, i)
   print (1,*), i       ! { dg-error "in PRINT statement" }
end program p

subroutine prn(a, b, c, i)
  integer i
  character(len=*) a, b, c
  print (a//(b//c)), i, i, i
  print trim(a//trim(b//c)), i, i, i
end subroutine prn
