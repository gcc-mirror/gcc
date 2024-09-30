!{ dg-do run }

! Contributed by Anton Shterenlikht  <as at cmplx dot uk>
! Check PR81265 is fixed.

module m
implicit none
private
public :: s

abstract interface
  subroutine halo_exchange( array )
    integer, allocatable, intent( inout ) :: array(:,:,:,:)[:,:,:]
  end subroutine halo_exchange
end interface

interface
  module subroutine s( coarray, hx )
    integer, allocatable, intent( inout ) :: coarray(:,:,:,:)[:,:,:]
    procedure( halo_exchange ) :: hx
  end subroutine s
end interface

end module m
submodule( m ) sm
contains
module procedure s

if ( .not. allocated(coarray) ) then
  write (*,*) "ERROR: s: coarray is not allocated"
  error stop
end if

sync all

call hx( coarray ) 

end procedure s

end submodule sm
module m2
  implicit none
  private
  public :: s2
  contains
    subroutine s2( coarray )
      integer, allocatable, intent( inout ) :: coarray(:,:,:,:)[:,:,:]
      if ( .not. allocated( coarray ) ) then
        write (*,'(a)') "ERROR: s2: coarray is not allocated"
        error stop
      end if
    end subroutine s2
end module m2
program p
use m
use m2
implicit none
integer, allocatable :: space(:,:,:,:)[:,:,:]
integer :: errstat

allocate( space(10,10,10,2) [2,2,*], source=0, stat=errstat )
if ( errstat .ne. 0 ) then
  write (*,*) "ERROR: p: allocate( space ) )"
  error stop
end if

if ( .not. allocated (space) ) then
  write (*,*) "ERROR: p: space is not allocated"
  error stop
end if

call s( space, s2 )

end program p
