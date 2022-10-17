! PR middle-end/45423 - for the original C/C++ testcase
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple -g0 -Wno-deprecated" }
! atomicvar should never be referenced in between the barrier and
! following #pragma omp atomic_load.
! { dg-final { scan-tree-dump-not "barrier\[^#\]*atomicvar" "gimple" } }

module m
  implicit none
  logical :: atomicvar, c
  integer :: i, atomicvar2, c2
contains
integer function foo ()
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .or. .true.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .or. .false.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .or. c
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .and. .true.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .and. .false.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .and. c
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .neqv. .true.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .neqv. .false.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .neqv. c
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .eqv. .true.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .eqv. .false.
  !$omp barrier
  !$omp atomic
    atomicvar = atomicvar .eqv. c
  !$omp barrier
  !$omp atomic
    atomicvar = .true. .or. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = .false. .or. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = c .or. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = .true. .and. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = .false. .and. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = c .and. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = .true. .neqv. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = .false. .neqv. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = c .neqv. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = .true. .eqv. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = .false. .eqv. atomicvar
  !$omp barrier
  !$omp atomic
    atomicvar = c .eqv. atomicvar
  !$omp barrier
  foo = 0
end

integer function bar ()
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (atomicvar2, -1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (atomicvar2, 0)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (atomicvar2, 1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (atomicvar2, 2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (atomicvar2, c2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (-1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (0, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (c2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (atomicvar2, -1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (atomicvar2, 0)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (atomicvar2, 1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (atomicvar2, 2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (atomicvar2, c2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (-1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (0, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ieor (1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = ior (c2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (atomicvar2, -1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (atomicvar2, 0)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (atomicvar2, 1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (atomicvar2, 2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (atomicvar2, c2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (-1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (0, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = iand (c2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (atomicvar2, -1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (atomicvar2, 0)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (atomicvar2, 1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (atomicvar2, 2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (atomicvar2, c2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (-1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (0, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = min (c2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (atomicvar2, -1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (atomicvar2, 0)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (atomicvar2, 1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (atomicvar2, 2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (atomicvar2, c2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (-1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (0, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (1, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = max (c2, atomicvar2)
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 + (-1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 + 0
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 + 1
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 + 2
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 + c2
  !$omp barrier
  !$omp atomic
    atomicvar2 = -1 + atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 0 + atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 1 + atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 2 + atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = c2 + atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 - (-1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 - 0
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 - 1
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 - 2
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 - c2
  !$omp barrier
  !$omp atomic
    atomicvar2 = -1 - atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 0 - atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 1 - atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 2 - atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = c2 - atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 * (-1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 * 0
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 * 1
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 * 2
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 * c2
  !$omp barrier
  !$omp atomic
    atomicvar2 = (-1) * atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 0 * atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 1 * atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 2 * atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = c2 * atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 / (-1)
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 / 0
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 / 1
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 / 2
  !$omp barrier
  !$omp atomic
    atomicvar2 = atomicvar2 / c2
  !$omp barrier
  !$omp atomic
    atomicvar2 = (-1) / atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 0 / atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 1 / atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = 2 / atomicvar2
  !$omp barrier
  !$omp atomic
    atomicvar2 = c2 / atomicvar2
  !$omp barrier
  bar = 0
end
end module
