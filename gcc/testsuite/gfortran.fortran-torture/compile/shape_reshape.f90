! This checks that the shape of the SHAPE intrinsic is known.
PROGRAM shape_reshape
   INTEGER, ALLOCATABLE :: I(:,:)
   ALLOCATE(I(2,2))
   I = RESHAPE((/1,2,3,4/),SHAPE=SHAPE(I))
   DEALLOCATE(I)
END PROGRAM

