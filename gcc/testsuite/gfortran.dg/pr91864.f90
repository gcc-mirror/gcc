program p
   integer :: i
   read (*,*) i%kind   ! { dg-error "Expecting variable or io-implied-do" }
end

subroutine t
   integer, allocatable :: x(:)
   integer :: stat
   allocate (x(3), stat=stat%kind)   ! { dg-error "cannot be a constant" }
end

subroutine u
   integer, allocatable :: x(:)
   integer :: stat
   allocate (x(3), stat%kind=stat)   ! { dg-error "Unexpected constant" }
end

subroutine v
   integer, allocatable :: x(:)
   integer :: stat
   deallocate (x, stat%kind=stat)   ! { dg-error "Unexpected constant" }
end
