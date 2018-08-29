! { dg-do run }
! { dg-additional-sources only_clause_main.c }
module testOnlyClause

  contains
    subroutine testOnly(cIntPtr) bind(c, name="testOnly")
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_f_pointer
      implicit none
      type(c_ptr), value :: cIntPtr
      integer(c_int), pointer :: f90IntPtr
      
      call c_f_pointer(cIntPtr, f90IntPtr)

      ! f90IntPtr coming in has value of -11; this will make it -12
      f90IntPtr = f90IntPtr - 1
      if(f90IntPtr .ne. -12) then
         STOP 1
      endif
    end subroutine testOnly
end module testOnlyClause
