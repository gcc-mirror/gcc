! Program to test the LEN and LEN_TRIM intrinsics.
subroutine test (c)
   character(*) c
   character(len(c)) d

   d = c
   if (len(d) .ne. 20) STOP 1
   if (d .ne. "Longer Test String") STOP 2
   c = "Hello World"
end subroutine

subroutine test2 (c)
   character (*) c
   character(len(c)) d

   d = c
   if (len(d) .ne. 6) STOP 3
   if (d .ne. "Foobar") STOP 4
end subroutine

program strlen
   implicit none
   character(20) c
   character(5) a, b
   integer i

   c = "Longer Test String"
   call test (c)

   if (len(c) .ne. 20) STOP 5
   if (len_trim(c) .ne. 11) STOP 6

   call test2 ("Foobar");
end program
