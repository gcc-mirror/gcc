! { dg-do compile }
subroutine foo1
   implicit none
   integer i
   open(1, iomsg=666)      ! { dg-error "IOMSG must be" }
   open(1, iomsg='sgk')    ! { dg-error "IOMSG must be" }
   open(1, iomsg=i)        ! { dg-error "IOMSG must be" }
   close(1, iomsg=666)     ! { dg-error "IOMSG must be" }
   close(1, iomsg='sgk')   ! { dg-error "IOMSG must be" }
   close(1, iomsg=i)       ! { dg-error "IOMSG must be" }
end subroutine foo1

subroutine foo
   implicit none
   integer i
   real :: x = 1
   write(1, *, iomsg='sgk') x   ! { dg-error "IOMSG must be" }
   write(1, *, iomsg=i)     x   ! { dg-error "IOMSG must be" }
   read(1,  *, iomsg='sgk') x   ! { dg-error "IOMSG must be" }
   read(1,  *, iomsg=i)     x   ! { dg-error "IOMSG must be" }
   flush(1,    iomsg='sgk')     ! { dg-error "IOMSG must be" }
   flush(1,    iomsg=i)         ! { dg-error "IOMSG must be" }
   rewind(1,   iomsg='sgk')     ! { dg-error "IOMSG must be" }
   rewind(1,   iomsg=i)         ! { dg-error "IOMSG must be" }
   backspace(1,iomsg='sgk')     ! { dg-error "IOMSG must be" }
   backspace(1,iomsg=i)         ! { dg-error "IOMSG must be" }
   wait(1,     iomsg='sgk')     ! { dg-error "IOMSG must be" }
   wait(1,     iomsg=i)         ! { dg-error "IOMSG must be" }
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
