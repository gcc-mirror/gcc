! { dg-do run }
!
! Test the fix for PR55763 comment 9 as part of the overall fix for PR34640.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
  program change_field_type
    use, intrinsic :: iso_c_binding
    implicit none
    REAL(kind=c_float), POINTER :: vector_comp(:)
    TYPE, BIND(C) :: scalar_vector
       REAL(kind=c_float) :: scalar
       REAL(kind=c_float) :: vec(3)
    END TYPE
    TYPE, BIND(C) :: scalar_vector_matrix
       REAL(kind=c_float) :: scalar
       REAL(kind=c_float) :: vec(3)
       REAL(kind=c_float) :: mat(3,3)
    END TYPE
    CLASS(*), ALLOCATABLE, TARGET :: one_d_field(:)
    real, pointer :: v1(:)

    allocate(one_d_field(3), &
             source = (/ scalar_vector( 1.0, (/ -1.0, 0.0, 1.0 /) ), &
                         scalar_vector( 1.1, (/ -1.2, 0.2, 0.9 /) ), &
                         scalar_vector( 1.2, (/ -1.4, 0.4, 0.8 /) )  /) )

    call extract_vec(one_d_field, 1, 2)
    if (any (abs (vector_comp - [0.0,0.2,0.4]) .gt. 1e-4)) call abort
    deallocate(one_d_field)   ! v1 becomes undefined

    allocate(one_d_field(1), &
         source = (/ scalar_vector_matrix( 1.0, (/ -1.0, 0.0, 1.0 /), &
         reshape( (/ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /), &
                 (/3, 3/) ) ) /) )

    call extract_vec(one_d_field, 2, 1)
    if (abs (vector_comp(1) + 1.0) > 1e-4) call abort
    call extract_vec(one_d_field, 2, 3)
    if (abs (vector_comp(1) - 1.0) > 1e-4) call abort
    deallocate(one_d_field)   ! v1 becomes undefined
  contains
    subroutine extract_vec(field, tag, ic)
        use, intrinsic :: iso_c_binding
        CLASS(*), TARGET :: field(:)
        INTEGER(kind=c_int), value :: tag, ic

        type(scalar_vector), pointer :: sv(:)
        type(scalar_vector_matrix), pointer :: svm(:)

        select type (field)
        type is (real(c_float))
          vector_comp => field
        class default
          select case (tag)
          case (1)
             sv => field
             vector_comp => sv(:)%vec(ic)
          case (2)
             svm => field
             vector_comp => svm(:)%vec(ic)
          end select
        end select
    end subroutine
  end program
