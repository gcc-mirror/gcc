! { dg-do run }
! { dg-options "-fbackslash" }
! PR41328 and PR41168 Improper read of CR-LF sequences.
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program main
   implicit none
   integer :: iostat, n_chars_read, k
   character(len=1) :: buffer(64) = ""
   character (len=80) :: u

   ! Set up the test file with normal file end.
   open(unit=10, file="crlftest", form="unformatted", access="stream",&
   & status="replace")
   write(10) "a\rb\rc\r" ! CR at the end of each record.
   close(10, status="keep")

   open(unit=10, file="crlftest", form="formatted", status="old")
   
   read( unit=10, fmt='(64A)', advance='NO', iostat=iostat,          &
         size=n_chars_read ) buffer
   if (n_chars_read.ne.1) STOP 1
   if (any(buffer(1:n_chars_read).ne."a")) STOP 2
   if (.not.is_iostat_eor(iostat)) STOP 3

   read( unit=10, fmt='(64A)', advance='NO', iostat=iostat,          &
         size=n_chars_read ) buffer
   if (n_chars_read.ne.1) STOP 4
   if (any(buffer(1:n_chars_read).ne."b")) STOP 5
   if (.not.is_iostat_eor(iostat)) STOP 6

   read( unit=10, fmt='(64A)', advance='NO', iostat=iostat,          &
         size=n_chars_read ) buffer
   if (n_chars_read.ne.1) STOP 7
   if (any(buffer(1:n_chars_read).ne."c")) STOP 8
   if (.not.is_iostat_eor(iostat)) STOP 9

   read( unit=10, fmt='(64A)', advance='NO', iostat=iostat,          &
         size=n_chars_read ) buffer
   if (n_chars_read.ne.0) STOP 10
   if (any(buffer(1:n_chars_read).ne."a")) STOP 11
   if (.not.is_iostat_end(iostat)) STOP 12
   close(10, status="delete")

   ! Set up the test file with normal file end.
   open(unit=10, file="crlftest", form="unformatted", access="stream",&
   & status="replace")
   write(10) "a\rb\rc\rno end of line marker" ! Note, no CR at end of file.
   close(10, status="keep")

   open(unit=10, file="crlftest", status='old')

   do k = 1, 10
     read(10,'(a80)',end=101,err=100) u
     !print *,k,' : ',u(1:len_trim(u))
   enddo
   
100 continue
   close(10, status="delete")
   STOP 13

101 continue
   close(10, status="delete")
   if (u(1:len_trim(u)).ne."no end of line marker") STOP 14
end program main
