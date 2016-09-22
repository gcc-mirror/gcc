! { dg-do compile }
! { dg-options -std=legacy }
!
! Test elimination of various segfaults and ICEs on error recovery.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
module m1
   type t
   end type
   interface write(formatted)
      module procedure s
   end interface
contains
   subroutine s(dtv,unit,iotype,vlist,extra,iostat,iomsg) ! { dg-error "Too many dummy arguments" }
      class(t), intent(in) :: dtv
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
   end
end

module m2
   type t
   end type
   interface read(formatted)
      module procedure s
   end interface
contains
   subroutine s(dtv,unit,iotype,vlist,iostat,iomsg,extra) ! { dg-error "Too many dummy arguments" }
      class(t), intent(inout) :: dtv
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
   end
end

module m3
   type t
   end type
   interface read(formatted)
      module procedure s
   end interface
contains
   subroutine s(dtv,extra,unit,iotype,vlist,iostat,iomsg) ! { dg-error "Too many dummy arguments" }
      class(t), intent(inout) :: dtv
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
   end
end

module m4
   type t
   end type
   interface write(unformatted)
      module procedure s
   end interface
contains
   subroutine s(*) ! { dg-error "Alternate return" }
   end
end

module m5
   type t
   contains
      procedure :: s
      generic :: write(unformatted) => s
   end type
contains
   subroutine s(dtv, *) ! { dg-error "Too few dummy arguments" }
      class(t), intent(out) :: dtv
   end
end

module m6
   type t
      character(len=20) :: name
      integer(4) :: age
   contains
      procedure :: pruf
      generic :: read(unformatted) => pruf
   end type
contains
   subroutine pruf (dtv,unit,*,iomsg) ! { dg-error "Alternate return" }
      class(t), intent(inout) :: dtv
      integer, intent(in) :: unit
      character(len=*), intent(inout) :: iomsg
      write (unit=unit, iostat=iostat, iomsg=iomsg) dtv%name, dtv%age
   end
end

module m7
   type t
      character(len=20) :: name
      integer(4) :: age
   contains
      procedure :: pruf
      generic :: read(unformatted) => pruf
   end type
contains
   subroutine pruf (dtv,unit,iostat) ! { dg-error "Too few dummy arguments" }
      class(t), intent(inout) :: dtv
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(len=1) :: iomsg
      write (unit=unit, iostat=iostat, iomsg=iomsg) dtv%name, dtv%age
   end
end

module m
   type t
      character(len=20) :: name
      integer(4) :: age
   contains
      procedure :: pruf
      generic :: read(unformatted) => pruf
   end type
contains
   subroutine pruf (dtv,unit,iostat,iomsg)
      class(t), intent(inout) :: dtv
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
      write (unit=unit, iostat=iostat, iomsg=iomsg) dtv%name, dtv%age
   end
end
program test
   use m
   character(3) :: a, b
   class(t) :: chairman ! { dg-error "must be dummy, allocatable or pointer" }
   open (unit=71, file='myunformatted_data.dat', form='unformatted')
! The following error is spurious and is eliminated if previous error is corrected.
! TODO Although better than an ICE, fix me.
   read (71) a, chairman, b ! { dg-error "cannot be polymorphic" }
   close (unit=71)
end

