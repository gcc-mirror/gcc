C { dg-do  run }
C { dg-options "-fcheck=bounds -fdump-tree-optimized -fblas-matmul-limit=1 -O -fexternal-blas" }
C { dg-additional-sources blas_gemm_routines.f }
C Test calling of BLAS routines

      program main
      call sub_s
      call sub_d
      call sub_c
      call sub_z
      end

      subroutine sub_d
      implicit none
      real(8), dimension(3,2) :: a
      real(8), dimension(2,3) :: at
      real(8), dimension(2,4) :: b
      real(8), dimension(4,2) :: bt
      real(8), dimension(3,4) :: c
      real(8), dimension(3,4) :: cres
      real(8), dimension(:,:), allocatable :: c_alloc
      data a / 2.,  -3.,  5.,  -7., 11., -13./
      data b /17., -23., 29., -31., 37., -39., 41., -47./
      data cres /195., -304.,  384.,  275., -428.,  548.,  347., -540.,
     &           692.,  411., -640.,  816./

      c = matmul(a,b)
      if (any (c /= cres)) stop 31

      at = transpose(a)
      c = (1.2,-2.2)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 32

      bt = transpose(b)
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 33

      c_alloc = matmul(a,b)
      if (any (c /= cres)) stop 34

      at = transpose(a)
      deallocate (c_alloc)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 35

      bt = transpose(b)
      allocate (c_alloc(20,20))
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 36

      end

      subroutine sub_s
      implicit none
      real, dimension(3,2) :: a
      real, dimension(2,3) :: at
      real, dimension(2,4) :: b
      real, dimension(4,2) :: bt
      real, dimension(3,4) :: c
      real, dimension(3,4) :: cres
      real, dimension(:,:), allocatable :: c_alloc
      data a / 2.,  -3.,  5.,  -7., 11., -13./
      data b /17., -23., 29., -31., 37., -39., 41., -47./
      data cres /195., -304.,  384.,  275., -428.,  548.,  347., -540.,
     &           692.,  411., -640.,  816./

      c = matmul(a,b)
      if (any (c /= cres)) stop 21

      at = transpose(a)
      c = (1.2,-2.2)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 22

      bt = transpose(b)
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 23

      c_alloc = matmul(a,b)
      if (any (c /= cres)) stop 24

      at = transpose(a)
      deallocate (c_alloc)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 25

      bt = transpose(b)
      allocate (c_alloc(20,20))
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 26

      end

      subroutine sub_c
      implicit none
      complex, dimension(3,2) :: a
      complex, dimension(2,3) :: at, ah
      complex, dimension(2,4) :: b
      complex, dimension(4,2) :: bt, bh
      complex, dimension(3,4) :: c
      complex, dimension(3,4) :: cres
      complex, dimension(:,:), allocatable :: c_alloc
      
      data a / (2.,-3.), (-5.,7.), (11.,-13.), (17.,19), (-23., -29),
     &     (-31., 37.)/

      data b / (-41., 43.), (-47., 53.), (-59.,-61.), (-67., 71),
     &     ( 73.,79. ), (83.,-89.), (97.,-101.), (-107.,-109.)/
      data cres /(-1759.,217.), (2522.,-358.), (-396.,-2376.),
     &     (-2789.,-11.),
     &     (4322.,202.), (-1992.,-4584.), (3485.,3.), (-5408.,-244.),
     &     (2550.,5750.), (143.,-4379.), (-478.,6794.), (7104.,-2952.) /

      c = matmul(a,b)
      if (any (c /= cres)) stop 1

      at = transpose(a)
      c = (1.2,-2.2)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 2

      bt = transpose(b)
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 3

      ah = transpose(conjg(a))
      c = (1.2,-2.2)
      c = matmul(conjg(transpose(ah)), b)
      if (any (c /= cres)) stop 4

      bh = transpose(conjg(b))
      c = (1.2,-2.2)
      c = matmul(a, transpose(conjg(bh)))
      if (any (c /= cres)) stop 5

      c_alloc = matmul(a,b)
      if (any (c /= cres)) stop 6

      at = transpose(a)
      deallocate (c_alloc)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 7

      bt = transpose(b)
      allocate (c_alloc(20,20))
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 8

      ah = transpose(conjg(a))
      c = (1.2,-2.2)
      c = matmul(conjg(transpose(ah)), b)
      if (any (c /= cres)) stop 9

      deallocate (c_alloc)
      allocate (c_alloc(0,0))
      bh = transpose(conjg(b))
      c = (1.2,-2.2)
      c = matmul(a, transpose(conjg(bh)))
      if (any (c /= cres)) stop 10

      end

      subroutine sub_z
      implicit none
      complex(8), dimension(3,2) :: a
      complex(8), dimension(2,3) :: at, ah
      complex(8), dimension(2,4) :: b
      complex(8), dimension(4,2) :: bt, bh
      complex(8), dimension(3,4) :: c
      complex(8), dimension(3,4) :: cres
      complex(8), dimension(:,:), allocatable :: c_alloc
      
      data a / (2.,-3.), (-5._8,7.), (11.,-13.), (17.,19),
     &         (-23., -29), (-31., 37.)/

      data b / (-41., 43.), (-47., 53.), (-59.,-61.), (-67., 71),
     &     ( 73.,79. ), (83.,-89.), (97.,-101.), (-107.,-109.)/
      data cres /(-1759.,217.), (2522.,-358.), (-396.,-2376.),
     &     (-2789.,-11.),
     &     (4322.,202.), (-1992.,-4584.), (3485.,3.), (-5408.,-244.),
     &     (2550.,5750.), (143.,-4379.), (-478.,6794.), (7104.,-2952.) /

      c = matmul(a,b)
      if (any (c /= cres)) stop 11

      at = transpose(a)
      c = (1.2,-2.2)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 12

      bt = transpose(b)
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 13

      ah = transpose(conjg(a))
      c = (1.2,-2.2)
      c = matmul(conjg(transpose(ah)), b)
      if (any (c /= cres)) stop 14

      bh = transpose(conjg(b))
      c = (1.2,-2.2)
      c = matmul(a, transpose(conjg(bh)))
      if (any (c /= cres)) stop 15

      c_alloc = matmul(a,b)
      if (any (c /= cres)) stop 16

      at = transpose(a)
      deallocate (c_alloc)
      c = matmul(transpose(at), b)
      if (any (c /= cres)) stop 17

      bt = transpose(b)
      allocate (c_alloc(20,20))
      c = (1.2,-2.1)
      c = matmul(a, transpose(bt))
      if (any (c /= cres)) stop 18

      ah = transpose(conjg(a))
      c = (1.2,-2.2)
      c = matmul(conjg(transpose(ah)), b)
      if (any (c /= cres)) stop 19

      deallocate (c_alloc)
      allocate (c_alloc(0,0))
      bh = transpose(conjg(b))
      c = (1.2,-2.2)
      c = matmul(a, transpose(conjg(bh)))
      if (any (c /= cres)) stop 20

      end
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "optimized" } }
