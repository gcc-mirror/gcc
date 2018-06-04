! { dg-do compile }
program p
   integer, allocatable :: arr(:)
   integer :: stat
   character(len=128, kind=4) :: errmsg = ' '
   allocate (arr(3), stat=stat, errmsg=errmsg)  ! { dg-error "shall be a scalar default CHARACTER" }
   print *, allocated(arr), stat, trim(errmsg)
end
