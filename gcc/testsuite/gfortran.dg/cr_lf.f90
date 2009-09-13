! { dg-do run }
! { dg-options "-fbackslash" }
! PR41328 and PR41168 Improper read of CR-LF sequences.
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program main
   implicit none
   integer :: iostat, n_chars_read
   character(len=1) :: buffer(64) = ""
   open( unit=10, form="formatted", access="stream", status="scratch")
   write(10, fmt="(a)", advance="no") "a\rb\rc\r"
   rewind(10)
   
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
end program main
