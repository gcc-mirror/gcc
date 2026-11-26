! { dg-additional-options "-fdump-tree-gimple" }

! For defaultmap:
! Check that 'type(t)' is an aggreate, unless it has a pointer or allocate attribute

subroutine sub(d,vd,od,ad,pd,oad,opd, da,oda,ada,pda,oada,opda,n)
  use iso_c_binding
  implicit none
  integer, value :: n
  type(c_ptr) :: d,vd,od,ad,pd,oad,opd, s, ps, as
  type(c_ptr) :: da(n),oda(n),ada(:),pda(:),oada(:),opda(:), sa(n), psa(:), asa(:)
  value :: vd
  optional :: od, oad, opd, oda, oada, opda
  pointer :: pd, opd, pda, opda, ps, psa
  allocatable :: ad, oad, ada, oada, as, asa

  ! all variables unallocated & not initialized - but that's sufficient for a dump test!

  ! non-ptr, non-alloc
  !$omp target defaultmap(firstprivate : aggregate) defaultmap(tofrom : scalar) defaultmap(present : pointer) defaultmap(alloc: allocatable)
  block
    logical :: pr
    pr =      c_associated(d) &   ! OK
         .or. c_associated(vd) &  ! OK
         .or. c_associated(od) &  ! OK
         .or. c_associated(s) &   ! OK
         .or. c_associated(da(1)) &  ! OK
         .or. c_associated(oda(1)) & ! OK
         .or. c_associated(sa(1)) ! OK
  end block

  ! ptr
  !$omp target defaultmap(present : aggregate) defaultmap(tofrom : scalar) defaultmap(firstprivate : pointer) defaultmap(alloc: allocatable)
  block
    logical :: pr
    pr =      c_associated(pd) &  ! OK
         .or. c_associated(opd) & ! OK
         .or. c_associated(ps) &  ! OK
         .or. c_associated(pda(1)) & ! wrong: aggregate but is pointer
         .or. c_associated(opda(1))& ! wrong: aggregate but is pointer
         .or. c_associated(psa(1))  ! wrong: aggregate but is pointer
  end block

  ! alloc
  !$omp target defaultmap(alloc : aggregate) defaultmap(tofrom : scalar) defaultmap(present : pointer) defaultmap(firstprivate: allocatable)
  block
    logical :: pr
    pr =      c_associated(ad) &   ! OK
         .or. c_associated(oad) &  ! OK
         .or. c_associated(as) &   ! OK
         .or. c_associated(ada(1)) &  ! -> wrong: aggregate is allocatable!
         .or. c_associated(oada(1)) & ! -> wrong: aggregate is allocatable!
         .or. c_associated(asa(1)) ! OK
  end block
end

! Except for some additional bookkeeping, all referenced variables should be 'firstprivate'

! { dg-final { scan-tree-dump "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) defaultmap\\(tofrom:scalar\\) defaultmap\\(firstprivate:aggregate\\) defaultmap\\(alloc:allocatable\\) defaultmap\\(present:pointer\\) \[^\\n\\r\]*firstprivate\\(sa\\) firstprivate\\(s\\) firstprivate\\(oda\\) firstprivate\\(da\\) firstprivate\\(od\\) firstprivate\\(vd\\) firstprivate\\(d\\)" "gimple" } }
! { dg-final { scan-tree-dump "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) defaultmap\\(tofrom:scalar\\) defaultmap\\(present:aggregate\\) defaultmap\\(alloc:allocatable\\) defaultmap\\(firstprivate:pointer\\) firstprivate\\(psa\\) firstprivate\\(ps\\) firstprivate\\(opda\\) firstprivate\\(pda\\) firstprivate\\(opd\\) firstprivate\\(pd\\)" "gimple" } }
! { dg-final { scan-tree-dump "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) defaultmap\\(tofrom:scalar\\) defaultmap\\(alloc:aggregate\\) defaultmap\\(firstprivate:allocatable\\) defaultmap\\(present:pointer\\) firstprivate\\(asa\\) firstprivate\\(as\\) firstprivate\\(oada\\) firstprivate\\(ada\\) firstprivate\\(oad\\) firstprivate\\(ad\\)" "gimple" } }
