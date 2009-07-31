subroutine foo ( uplo, ap, y )
      character*1        uplo
      complex(kind((1.0d0,1.0d0)))         ap( * ), y( * )
      if     ( .not. scan( uplo, 'uu' )>0.and. &
         .not. scan( uplo, 'll' )>0      )then
            do 60, j = 1, n
               y( j ) = y( j ) + dble( ap( kk ) )
               kk     = kk     + j
   60       continue
      end if
      end
