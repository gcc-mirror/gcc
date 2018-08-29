! { dg-do compile }
! PR 41197
program main
  integer, dimension (4) :: ier = 0
  character(len=30), dimension(2) :: er
  integer, dimension (:), allocatable :: a
  allocate (a (16), stat = ier) ! { dg-error "must be a scalar INTEGER" }
  allocate (a (14), stat=ier(1),errmsg=er) ! { dg-error "shall be a scalar default CHARACTER" }
end

