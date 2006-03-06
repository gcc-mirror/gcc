! { dg-do compile }
! This checks the fix for PR 26041.
!
! Contributed by H.J. Lu  <hongjiu.lu@intel.com>
module foo
   public    bar_
   interface bar_
      module procedure bar
   end interface
   public    xxx_
   interface xxx_
      module procedure xxx
   end interface
contains
   subroutine bar(self, z)
      interface
         function self(z) result(res)
        real z
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
   end subroutine
   subroutine xxx(self,z)
      interface
         function self(z) result(res)
        real z
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      call bar(self, z)
   end subroutine
end
