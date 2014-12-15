! { dg-do compile }
! Tests fix for PR26107 in which an ICE would occur after the second
! error message below.  This resulted from a spurious attempt to
! produce the third error message, without the name of the function.
!
! This is an expanded version of the testcase in the PR.
!
   pure function equals(self, &     ! { dg-error "must be INTENT" }
                        string, ignore_case) result(same)
         character(*), intent(in) :: string
         integer(4), intent(in) :: ignore_case
         integer(4) :: same
         if (len (self) < 1) return ! { dg-error "must be CHARACTER" }
         same = 1
   end function

   function impure(self) result(ival)
         character(*), intent(in) :: self
         ival = 1
   end function

   pure function purity(self, string, ignore_case) result(same)
         character(*), intent(in) :: self
         character(*), intent(in) :: string
         integer(4), intent(in) :: ignore_case
         integer i
         if (end > impure (self)) & ! { dg-error "impure function" }
           return
   end function
