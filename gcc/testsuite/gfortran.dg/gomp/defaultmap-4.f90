! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-gimple" }
!
! PR fortran/92568
!
implicit none
  type t
  end type t

  integer :: ii
  integer :: arr(5)
  integer, allocatable :: aii, aarr(:)
  integer, pointer :: pii, parr(:)

  character :: str1, str1arr(5), str1a, str1aarr(:), str1p, str1parr(:)
  character(len=5) :: str5, str5arr(5), str5a, str5aarr(:), str5p, str5parr(:)
  character(len=:) :: strXa, strXaarr(:), strXp, strXparr(:)
  allocatable :: str1a, str1aarr, str5a, str5aarr, strXa, strXaarr
  pointer :: str1p, str1parr, str5p, str5parr, strXp, strXparr

  type(t) :: dt, dtarr(5), dta, dtaarr(:), dtp, dtparr(:)
  allocatable :: dta, dtaarr
  pointer :: dtp, dtparr

  allocate(aii, aarr(5), str1a, str1aarr(5), dta, dtparr(5))
  allocate(pii, parr(5), str1p, str1parr(5), dtp, dtparr(5))
  allocate(character(len=7) :: strXa, strXaarr(5), strXp, strXparr(5))


  !$omp target defaultmap ( alloc )
    ii = 42; arr = 42; aii = 42; aarr = 42; pii = 42; parr = 42
    str1 = ""; str1arr = ""; str1a = ""; str1aarr = ""; str1p = ""; str1parr = ""
    str5 = ""; str5arr = ""; str5a = ""; str5aarr = ""; str5p = ""; str5parr = ""
    strXa = ""; strXaarr = ""; strXp = ""; strXparr = ""
    dt = t(); dtarr = t(); dta = t(); dtaarr = t(); dtp = t(); dtparr = t()
  !$omp end target

  !$omp target defaultmap(alloc : scalar)  defaultmap(to : aggregate)  &
  !$omp&       defaultmap(tofrom : allocatable) defaultmap(firstprivate : pointer)
    ii = 42; arr = 42; aii = 42; aarr = 42; pii = 42; parr = 42
    str1 = ""; str1arr = ""; str1a = ""; str1aarr = ""; str1p = ""; str1parr = ""
    str5 = ""; str5arr = ""; str5a = ""; str5aarr = ""; str5p = ""; str5parr = ""
    strXa = ""; strXaarr = ""; strXp = ""; strXparr = ""
    dt = t(); dtarr = t(); dta = t(); dtaarr = t(); dtp = t(); dtparr = t()
  !$omp end target
end

! { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(alloc:scalar\\) defaultmap\\(to:aggregate\\) defaultmap\\(tofrom:allocatable\\) defaultmap\\(firstprivate:pointer\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(alloc\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "firstprivate\\(dtp\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(pii\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(str1p\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(str5p\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(strxp\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*aii \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:aii \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:arr \\\[len:" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:\\.strxparr\\\] \\*\\) strxparr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:\\.strxaarr\\\] \\* restrict\\) strxaarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:5\\\] \\*\\) str5parr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:5\\\] \\* restrict\\) str5aarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:1\\\] \\*\\) str1parr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:1\\\] \\* restrict\\) str1aarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) parr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(struct t\\\[0:\\\] \\*\\) dtparr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(struct t\\\[0:\\\] \\* restrict\\) dtaarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) aarr\\.data \\\[len:" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "map\\(alloc:\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:1\\\] \\* restrict\\) str1aarr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:5\\\] \\* restrict\\) str5aarr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:\\.strxaarr\\\] \\* restrict\\) strxaarr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*dta \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:dta \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:dtarr \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:dt \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*dtp \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:dtp \\\[pointer assign, bias: 0\\\]\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:ii \\\[len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) aarr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*pii \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:pii \\\[pointer assign, bias: 0\\\]\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*str1a \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str1a \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str1arr \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str1 \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*str1p \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str1p \\\[pointer assign, bias: 0\\\]\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*str5a \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str5a \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str5arr \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str5 \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*str5p \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:str5p \\\[pointer assign, bias: 0\\\]\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\(struct t\\\[0:\\\] \\* restrict\\) dtaarr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*strxa \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:strxa \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:\\*strxp \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(alloc:strxp \\\[pointer assign, bias: 0\\\]\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(always_pointer:\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:1\\\] \\*\\) str1parr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(always_pointer:\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:5\\\] \\*\\) str5parr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(always_pointer:\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:\\.strxparr\\\] \\*\\) strxparr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(always_pointer:\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) parr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(always_pointer:\\(struct t\\\[0:\\\] \\*\\) dtparr\\.data \\\[pointer assign, bias: 0\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:aarr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:arr \\\[len:" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "map\\(to:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:\\.strxparr\\\] \\*\\) strxparr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:5\\\] \\*\\) str5parr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:1\\\] \\*\\) str1parr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) parr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\*\\(struct t\\\[0:\\\] \\*\\) dtparr\\.data \\\[len:" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "map\\(to:dtaarr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:dtarr \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:dt \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:dtparr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*aii \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:\\.strxaarr\\\] \\* restrict\\) strxaarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:5\\\] \\* restrict\\) str5aarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*\\(character\\(kind=1\\)\\\[0:\\\]\\\[1:1\\\] \\* restrict\\) str1aarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*\\(struct t\\\[0:\\\] \\* restrict\\) dtaarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) aarr\\.data \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*dta \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*str1a \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*str5a \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*strxa \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:parr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str1aarr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str1arr \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str1 \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str1parr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str5aarr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str5arr \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str5 \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:str5parr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\.strxaarr \\\[len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:strxaarr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\.strxa \\\[len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\.strxparr \\\[len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:strxparr \\\[pointer set, len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(to:\\.strxp \\\[len:" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) defaultmap\\(alloc\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) defaultmap\\(alloc:scalar\\) defaultmap\\(to:aggregate\\) defaultmap\\(tofrom:allocatable\\) defaultmap\\(firstprivate:pointer\\)" 1 "gimple" } }
