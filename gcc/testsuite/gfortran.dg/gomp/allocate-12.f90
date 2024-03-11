module m
  implicit none
contains
subroutine f ()
  !$omp declare target
  integer :: var  ! { dg-error "'allocate' directive for 'var' inside a target region must specify an 'allocator' clause" }
  !$omp allocate(var)
  var = 5
end

subroutine h ()
  !$omp target
   !$omp parallel
    !$omp single
       block
       integer :: var2(5)  ! { dg-error "'allocate' directive for 'var2' inside a target region must specify an 'allocator' clause" }
         !$omp allocate(var2)
         var2(1) = 7
       end block
    !$omp end single
   !$omp end parallel
  !$omp end target  
end
end module
