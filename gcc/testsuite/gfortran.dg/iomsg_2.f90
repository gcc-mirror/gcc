! { dg-do compile }
subroutine foo1
   implicit none
   integer i
   open(1, iomsg=666)      ! { dg-error "must be of type CHARACTER" }
   open(1, iomsg='sgk')    ! { dg-error "Non-variable expression" }
   open(1, iomsg=i)        ! { dg-error "must be of type CHARACTER" }
   close(1, iomsg=666)     ! { dg-error "must be of type CHARACTER" }
   close(1, iomsg='sgk')   ! { dg-error "Non-variable expression" }
   close(1, iomsg=i)       ! { dg-error "must be of type CHARACTER" }
end subroutine foo1

subroutine foo
   implicit none
   integer i
   real :: x = 1
   write(1, *, iomsg='sgk') x   ! { dg-error "Non-variable expression" }
   write(1, *, iomsg=i)     x   ! { dg-error "must be of type CHARACTER" }
   read(1,  *, iomsg='sgk') x   ! { dg-error "Non-variable expression" }
   read(1,  *, iomsg=i)     x   ! { dg-error "must be of type CHARACTER" }
   flush(1,    iomsg='sgk')     ! { dg-error "Non-variable expression" }
   flush(1,    iomsg=i)         ! { dg-error "must be of type CHARACTER" }
   rewind(1,   iomsg='sgk')     ! { dg-error "Non-variable expression" }
   rewind(1,   iomsg=i)         ! { dg-error "must be of type CHARACTER" }
   backspace(1,iomsg='sgk')     ! { dg-error "Non-variable expression" }
   backspace(1,iomsg=i)         ! { dg-error "must be of type CHARACTER" }
   wait(1,     iomsg='sgk')     ! { dg-error "Non-variable expression" }
   wait(1,     iomsg=i)         ! { dg-error "must be of type CHARACTER" }
end subroutine foo

subroutine bar
   implicit none
   integer i
   real :: x = 1
   character(len=20) s(2)
   open(1, iomsg=s)         ! { dg-error "must be scalar" }
   close(1, iomsg=s)        ! { dg-error "must be scalar" }
   write(1, *, iomsg=s) x   ! { dg-error "must be scalar" }
   read(1,  *, iomsg=s) x   ! { dg-error "must be scalar" }
   flush(1,    iomsg=s)     ! { dg-error "must be scalar" }
   rewind(1,   iomsg=s)     ! { dg-error "must be scalar" }
   backspace(1,iomsg=s)     ! { dg-error "must be scalar" }
   wait(1,     iomsg=s)     ! { dg-error "must be scalar" }
end subroutine bar
