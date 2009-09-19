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
   if (n_chars_read.ne.1) call abort
   if (any(buffer(1:n_chars_read).ne."a")) call abort
   if (.not.is_iostat_eor(iostat)) call abort

   read( unit=10, fmt='(64A)', advance='NO', iostat=iostat,          &
         size=n_chars_read ) buffer
   if (n_chars_read.ne.1) call abort
   if (any(buffer(1:n_chars_read).ne."b")) call abort
   if (.not.is_iostat_eor(iostat)) call abort

   read( unit=10, fmt='(64A)', advance='NO', iostat=iostat,          &
         size=n_chars_read ) buffer
   if (n_chars_read.ne.1) call abort
   if (any(buffer(1:n_chars_read).ne."c")) call abort
   if (.not.is_iostat_eor(iostat)) call abort

   read( unit=10, fmt='(64A)', advance='NO', iostat=iostat,          &
         size=n_chars_read ) buffer
   if (n_chars_read.ne.0) call abort
   if (any(buffer(1:n_chars_read).ne."a")) call abort
   if (.not.is_iostat_end(iostat)) call abort
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
   call abort

101 continue
   close(10, status="delete")
   if (u(1:len_trim(u)).ne."no end of line marker") call abort
end program main
