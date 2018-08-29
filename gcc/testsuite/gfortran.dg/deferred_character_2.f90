! { dg-do run }
!
! Tests the fix for PR68216
!
! Reported on clf: https://groups.google.com/forum/#!topic/comp.lang.fortran/eWQTKfqKLZc
!
PROGRAM hello
!
! This is based on the first testcase, from Francisco (Ayyy LMAO). Original
! lines are commented out. The second testcase from this thread is acalled
! at the end of the program.
!
    IMPLICIT NONE

    CHARACTER(LEN=:),DIMENSION(:),ALLOCATABLE :: array_lineas
    CHARACTER(LEN=:),DIMENSION(:),ALLOCATABLE :: array_copia
    character (3), dimension (2) :: array_fijo = ["abc","def"]
    character (100) :: buffer
    INTEGER :: largo , cant_lineas , i

    write (buffer, "(2a3)") array_fijo

!    WRITE(*,*) ' Escriba un numero para el largo de cada linea'
!    READ(*,*) largo
    largo = LEN (array_fijo)

!    WRITE(*,*) ' Escriba la cantidad de lineas'
!    READ(*,*) cant_lineas
    cant_lineas = size (array_fijo, 1)

    ALLOCATE(CHARACTER(LEN=largo) :: array_lineas(cant_lineas))

!    WRITE(*,*) 'Escriba el array', len(array_lineas), size(array_lineas)
    READ(buffer,"(2a3)") (array_lineas(i),i=1,cant_lineas)

!    WRITE(*,*) 'Array guardado: '
!    DO i=1,cant_lineas
!    WRITE(*,*) array_lineas(i)
!    ENDDO
     if (any (array_lineas .ne. array_fijo)) STOP 1

! The following are additional tests beyond that of the original.
!
! Check that allocation with source = another deferred length is OK
     allocate (array_copia, source = array_lineas)
     if (any (array_copia .ne. array_fijo)) STOP 2
     deallocate (array_lineas, array_copia)

! Check that allocation with source = a non-deferred length is OK
     allocate (array_lineas, source = array_fijo)
     if (any (array_lineas .ne. array_fijo)) STOP 3
     deallocate (array_lineas)

! Check that allocation with MOLD = a non-deferred length is OK
     allocate (array_copia, mold = [array_fijo(:)(1:2), array_fijo(:)(1:2)])
     if (size (array_copia, 1) .ne. 4) STOP 4
     if (LEN (array_copia, 1) .ne. 2) STOP 5

! Check that allocation with MOLD = another deferred length is OK
     allocate (array_lineas, mold = array_copia)
     if (size (array_copia, 1) .ne. 4) STOP 6
     if (LEN (array_copia, 1) .ne. 2) STOP 7
     deallocate (array_lineas, array_copia)

!    READ(*,*)
     call testdefchar
contains
     subroutine testdefchar
!
! This is the testcase in the above thread from Blokbuster
!
          implicit none
          character(:), allocatable :: test(:)

          allocate(character(3) :: test(2))
          test(1) = 'abc'
          test(2) = 'def'
          if (any (test .ne. ['abc', 'def'])) STOP 8

          test = ['aa','bb','cc']
          if (any (test .ne. ['aa', 'bb', 'cc'])) STOP 9

     end subroutine testdefchar

END PROGRAM
