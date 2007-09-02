! PR fortran/33276
! this used to crash due to an uninitialized variable in expand_iterator.

module foo
   type buffer_type
   integer(kind=kind(1)) :: item_end
   character(256) :: string
   end type
   type textfile_type
   type(buffer_type) :: buffer
   end type
contains
   function rest_of_line(self) result(res)
    type(textfile_type) :: self
     intent(inout) :: self
     character(128) :: res
     res = self%buffer%string(self%buffer%item_end+1: )
   end function

   subroutine read_intvec_ptr(v)
      integer(kind=kind(1)), dimension(:), pointer :: v
      integer(kind=kind(1)) :: dim,f,l,i

     if (dim>0) then; v = (/ (i, i=f,l)    /)
     end if
   end subroutine
end
