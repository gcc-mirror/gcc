! { dg-do run }
! { dg-additional-options "-O2" }
!
! PR fortran/128656 - ALLOCATE with STATUS and overflow of array size
!
! Reported by Henri Menke < henri at henrimenke.de >

program gfortran_overflow_errmsg
  use, intrinsic :: iso_c_binding,   only: C_INTPTR_T
  use, intrinsic :: iso_fortran_env, only: int64, real64
  implicit none
  real(real64), allocatable :: a(:), b(:)
  real(real64), pointer     :: c(:), d(:)
  integer(int64)            :: n
  integer                   :: stat1, stat2
  character(len=100)        :: msg = ""

  n = huge (1_C_INTPTR_T) / 2 + 1       ! 2**62 for 64 bit, 2**30 for 32 bit

  ! allocate of unallocated variable with overflow of size calculated
  ! gfortran could report a bogus "Attempt to allocate an allocated object"
  allocate(a(n), stat=stat1, errmsg=msg)

  ! error code for "Insufficient virtual memory"
  if (stat1 /= 5020) then
     print '(a,i0,2a)', 'stat=', stat1, ' errmsg=', trim(msg)
     stop 1
  end if

  allocate(c(n), stat=stat1, errmsg=msg)
  if (stat1 /= 5020) then
     print '(a,i0,2a)', 'stat=', stat1, ' errmsg=', trim(msg)
     stop 2
  end if

  ! For pointers, there is no "already allocated variable" error
  allocate(d(1))
  allocate(d(n), stat=stat2, errmsg=msg)
  if (stat2 /= 5020) then
     print '(a,i0,2a)', 'stat=', stat2, ' errmsg=', trim(msg)
     stop 3
  end if

  allocate(b(1))
  ! allocate of already allocated variable: shall give appropriate error
  allocate(b(n), stat=stat2, errmsg=msg)

  ! error code for "Attempt to allocate an allocated object"
  if (stat2 /= 5014) then
     print '(a,i0,2a)', 'stat=', stat2, ' errmsg=', trim(msg)
     stop 4
  end if
end program gfortran_overflow_errmsg
