! Program to test the LEN and LEN_TRIM intrinsics.
subroutine test (c)
   character(*) c
   character(len(c)) d

   d = c
   if (len(d) .ne. 20) call abort
   if (d .ne. "Longer Test String") call abort
   c = "Hello World"
end subroutine

subroutine test2 (c)
   character (*) c
   character(len(c)) d

   d = c
   if (len(d) .ne. 6) call abort
   if (d .ne. "Foobar") call abort
end subroutine

program strlen
   implicit none
   character(20) c
   character(5) a, b
   integer i

   c = "Longer Test String"
   call test (c)

   if (len(c) .ne. 20) call abort
   if (len_trim(c) .ne. 11) call abort

   call test2 ("Foobar");
end program
