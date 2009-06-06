! { dg-do "run" }

  ! Simplifier of RESHAPE was broken when reshaping an empty array.
  INTEGER, PARAMETER :: empty(0,0) = RESHAPE(SHAPE(1), (/0, 0/))

  ! same with surplus padding
  INTEGER, PARAMETER :: empty_padding(0,0) = RESHAPE(SHAPE(1), (/0, 0/), PAD=( (/ 1, 2 /) ))

  ! same with required padding
  INTEGER, PARAMETER :: non_empty(2,2) = RESHAPE(SHAPE(1), (/2, 2/), PAD=( (/ 1, 2 /) ))
END
