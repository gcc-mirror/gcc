! { dg-do  run }
! { dg-options "-finline-matmul-limit=0" }
! PR 86704 - this used to segfault.

program testmaticovenasobeni
implicit none
  
  character(len=10) :: line
  write (unit=line,fmt=*) testmatmul(120,1,3)

  contains

   function testmatmul(m,n,o)
     integer, intent(in) :: m,n,o
     real    :: A(n,m),B(n,o),C(m,o)
     logical :: testmatmul
    
     call random_number(A)
     call random_number(B)
     
     C=matmul(transpose(A),B) 
     testmatmul=.true.
   end function 

end program testmaticovenasobeni
