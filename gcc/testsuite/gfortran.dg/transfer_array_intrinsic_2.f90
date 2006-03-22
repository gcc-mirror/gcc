! { dg-do run }
! { dg-options "-fpack-derived" }
   call test3()
contains
   subroutine test3 ()
     type mytype
       sequence
       real(8) :: x = 3.14159
       character(4) :: ch = "wxyz"
       integer(2) :: i = 77
     end type mytype
     type(mytype) :: z(2)
     character(1) :: c(32)
     character(4) :: chr
     real(8) :: a
     integer(2) :: l
     equivalence (a, c(15)), (chr, c(23)), (l, c(27))
     c = transfer(z, c)
     if (a .ne. z(1)%x) call abort ()
     if (chr .ne. z(1)%ch) call abort ()
     if (l .ne. z(1)%i) call abort ()
   end subroutine test3
end
