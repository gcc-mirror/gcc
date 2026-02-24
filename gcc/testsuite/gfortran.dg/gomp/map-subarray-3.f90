! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! PR fortran/120505

! Check that mapping nested allocatable DT components triggers required
! additional mappings for the outer array descriptor.

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
chunk%tiles(1)%field%density0 = reshape([1,2,3,4],[2,2])
chunk%tiles(1)%field%density1 = reshape([5,6,7,8],[2,2])
allocate(chunk%left_rcv_buffer(1))

!$omp target enter data &
!$omp   map(to: chunk%tiles(1)%field%density0) &
!$omp   map(to: chunk%tiles(1)%field%density1) &
!$omp   map(to: chunk%left_rcv_buffer)

! { dg-final { scan-tree-dump-times { #pragma omp target enter data map\(alloc:\*\(struct tile_type\[0:\] \* restrict\) chunk\.tiles\.data \[len: 0\] \[runtime_implicit\] \[gimple only\]\) map\(to:chunk\.tiles \[pointer set, len: (?:36|64)\]\) map\(attach_detach:\(struct tile_type\[0:\] \* restrict\) chunk\.tiles\.data \[bias: 0 \(needs adjustment\)\]\) } 1 "original" } }

!$omp target exit data &
!$omp   map(from: chunk%tiles(1)%field%density0) &
!$omp   map(from: chunk%tiles(1)%field%density1) &
!$omp   map(from: chunk%left_rcv_buffer)

! { dg-final { scan-tree-dump-times { #pragma omp target exit data map\(release:chunk\.tiles \[pointer set, len: (?:36|64)\]\) map\(attach_detach:\(struct tile_type\[0:\] \* restrict\) chunk\.tiles\.data \[bias: 0 \(needs adjustment\)\]\) } 1 "original" } }

end
