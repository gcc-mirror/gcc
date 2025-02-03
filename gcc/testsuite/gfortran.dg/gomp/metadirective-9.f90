! { dg-do compile }

program OpenMP_Metadirective_WrongEnd_Test
  implicit none

  integer :: &
  iaVS, iV, jV, kV
  integer, dimension ( 3 ) :: &
    lV, uV
  logical :: &
    UseDevice

    !$OMP metadirective &
    !$OMP   when ( user = { condition ( UseDevice ) } &
    !$OMP     : target teams distribute parallel do simd collapse ( 3 ) &
    !$OMP         private ( iaVS ) ) &
    !$OMP   default ( parallel do simd collapse ( 3 ) private ( iaVS ) )
    do kV = lV ( 3 ), uV ( 3 )
      do jV = lV ( 2 ), uV ( 2 )
        do iV = lV ( 1 ), uV ( 1 )


        end do
      end do
    end do
    !$OMP end target teams distribute parallel do simd ! { dg-error "Unexpected !.OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD statement in OMP METADIRECTIVE block at .1." }


end program

