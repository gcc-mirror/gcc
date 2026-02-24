! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! PR fortran/120505

! Check that the bias into the inner derived type is correctly computed on 
! target enter data. For target exit data, the bias is ignored so just check
! that detach is present.
! Pointer set lengths are checked for both 32 and 64 bits.

module m

  type field_type
    real(kind=8), allocatable :: density0(:,:), density1(:,:)
  end type field_type

  type tile_type
    type(field_type) :: field
  end type tile_type

  type chunk_type
    real(kind=8), allocatable :: left_rcv_buffer(:)
    type(tile_type), allocatable :: tiles(:)
  end type chunk_type

  type(chunk_type) :: chunk

end

use m

allocate(chunk%tiles(1))
chunk%tiles(1)%field%density1 = reshape([1,2,3,4],[2,2])
allocate(chunk%left_rcv_buffer(1))

!$omp target enter data &
!$omp   map(to: chunk%tiles(1)%field%density1) &
!$omp   map(to: chunk%left_rcv_buffer)

! { dg-final { scan-tree-dump-times { map\(struct_unord:MEM <struct tile_type\[0:\]> \[\(struct tile_type\[0:\] \*\)_[0-9]+\] \[len: 1\]\) map\(to:MEM <struct tile_type\[0:\]> \[\(struct tile_type\[0:\] \*\)_[0-9]+\]\[_[0-9]+\]\.field\.density1 \[pointer set, len: (?:48|88)\]\) map\(attach:chunk\.tiles\.data \[bias: _[0-9]+\]\) } 1 "gimple" } }

!$omp target exit data &
!$omp   map(from: chunk%tiles(1)%field%density1) &
!$omp   map(from: chunk%left_rcv_buffer)

! { dg-final { scan-tree-dump-times { map\(release:chunk\.tiles \[pointer set, len: (?:36|64)\]\) map\(detach:chunk\.tiles\.data \[bias: [0-9]+\]\)} 1 "gimple" } }


! { dg-final { scan-tree-dump-not { map\(alloc } "gimple" } }
! { dg-final { scan-tree-dump-not { gimple only } "gimple" } }
! { dg-final { scan-tree-dump-not { needs adjustment } "gimple" } }

end
