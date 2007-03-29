! { dg-do run }
! { dg-options "-std=legacy" }
! PR27634 Missing period in format specifier. Test case derived from case given
! in PR.  Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      real          :: aval = 3.14
      character(6)  :: str = "xyz"
      character(12) :: input = "1234abcdef"
      character(8)  :: fmtstr = "(f4,a6)"
      aval = 0.0
      str = "xyz"
      read(input,fmtstr) aval, str
      if (aval.ne.1234.0) call abort()
      if (str.ne."abcdef") call abort()
      end

