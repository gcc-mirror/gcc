! { dg-compile }
! PR27634 Missing period in format specifier. Test case derived from case given
! in PR.  Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      real      aval
      character(6)  :: str
      character(12) :: input = "1234abcdef"
      read(input,'(f4,a6)') aval, str  !{ dg-error "Period required" }
      read(input,'(d10,a6)') aval, str !{ dg-error "Period required" }
      end

