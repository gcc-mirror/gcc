! PR fortran/102086

implicit none (type, external)
contains
subroutine as(a)
  type(*) :: a(:,:)
end
subroutine ar(b)
  type(*) :: b(..)
end
subroutine bar(x,y)
  type(*) :: x
  type(*) :: y(3,*)
  call as(x)  ! { dg-error "Rank mismatch in argument 'a' at .1. \\(rank-2 and scalar\\)" }
  call ar(x)  ! { dg-error "Assumed-type actual argument at .1. corresponding to assumed-rank dummy argument 'b' must be assumed-shape or assumed-rank" }
  call ar(y)  ! { dg-error "Assumed-type actual argument at .1. corresponding to assumed-rank dummy argument 'b' must be assumed-shape or assumed-rank" }
  call as(y(1,3))  ! { dg-error "Assumed-type variable y at .1. shall not have a subobject reference" }
  call ar(y(1,3))  ! { dg-error "Assumed-type variable y at .1. shall not have a subobject reference" }
  call as(y(1:1,3:3))  ! { dg-error "Assumed-type variable y at .1. shall not have a subobject reference" }
  call ar(y(1:1,3:3))  ! { dg-error "Assumed-type variable y at .1. shall not have a subobject reference" }
end

subroutine okayish(x,y,z)
  type(*) :: x(:)
  type(*) :: y(:,:)
  type(*) :: z(..)
  call as(x) ! { dg-error "Rank mismatch in argument 'a' at .1. \\(rank-2 and rank-1\\)" }
  call as(y) 
  call as(z) ! { dg-error "The assumed-rank array at .1. requires that the dummy argument 'a' has assumed-rank" }
  call ar(x)
  call ar(y)
  call ar(z)
end
end
