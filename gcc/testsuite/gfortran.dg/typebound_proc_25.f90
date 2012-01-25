! { dg-do compile }
!
! PR fortran/51995
!
! Contributed by jilfa12@yahoo.com
!

MODULE factory_pattern

  TYPE CFactory
     PRIVATE
     CHARACTER(len=20) :: factory_type      !! Descriptive name for database
     CLASS(Connection), POINTER :: connection_type !! Which type of database ?
   CONTAINS                                        !! Note 'class' not 'type' !
     PROCEDURE :: init                             !! Constructor
     PROCEDURE :: create_connection                !! Connect to database
     PROCEDURE :: finalize                         !! Destructor
  END TYPE CFactory

  TYPE, ABSTRACT :: Connection
   CONTAINS
     PROCEDURE(generic_desc), DEFERRED, PASS(self) :: description
  END TYPE Connection

  ABSTRACT INTERFACE
     SUBROUTINE generic_desc(self)
       IMPORT :: Connection
       CLASS(Connection), INTENT(in) :: self
     END SUBROUTINE generic_desc
  END INTERFACE

  !! An Oracle connection
  TYPE, EXTENDS(Connection) :: OracleConnection
   CONTAINS
     PROCEDURE, PASS(self) :: description => oracle_desc
  END TYPE OracleConnection

  !! A MySQL connection
  TYPE, EXTENDS(Connection) :: MySQLConnection
   CONTAINS
     PROCEDURE, PASS(self) :: description => mysql_desc
  END TYPE MySQLConnection

CONTAINS

  SUBROUTINE init(self, string)
    CLASS(CFactory), INTENT(inout) :: self
    CHARACTER(len=*), INTENT(in) :: string
    self%factory_type = TRIM(string)
    self%connection_type => NULL()            !! pointer is nullified
  END SUBROUTINE init

  SUBROUTINE finalize(self)
    CLASS(CFactory), INTENT(inout) :: self
    DEALLOCATE(self%connection_type)          !! Free the memory
    NULLIFY(self%connection_type)
  END SUBROUTINE finalize

  FUNCTION create_connection(self)  RESULT(ptr)
    CLASS(CFactory) :: self
    CLASS(Connection), POINTER :: ptr

    IF(self%factory_type == "Oracle") THEN
       IF(ASSOCIATED(self%connection_type))   DEALLOCATE(self%connection_type)
       ALLOCATE(OracleConnection :: self%connection_type)
       ptr => self%connection_type
    ELSEIF(self%factory_type == "MySQL") THEN
       IF(ASSOCIATED(self%connection_type))   DEALLOCATE(self%connection_type)
       ALLOCATE(MySQLConnection :: self%connection_type)
       ptr => self%connection_type
    END IF

  END FUNCTION create_connection

  SUBROUTINE oracle_desc(self)
    CLASS(OracleConnection), INTENT(in) :: self
    WRITE(*,'(A)') "You are now connected with Oracle"
  END SUBROUTINE oracle_desc

  SUBROUTINE mysql_desc(self)
    CLASS(MySQLConnection), INTENT(in) :: self
    WRITE(*,'(A)')  "You are now connected with MySQL"
  END SUBROUTINE mysql_desc
end module


  PROGRAM main
   USE factory_pattern

   IMPLICIT NONE

   TYPE(CFactory) :: factory
   CLASS(Connection), POINTER :: db_connect => NULL()

   CALL factory%init("Oracle")
   db_connect => factory%create_connection()   !! Create Oracle DB
   CALL db_connect%description()

   !! The same factory can be used to create different connections
   CALL factory%init("MySQL")                  !! Create MySQL DB

   !! 'connect' is a 'class' pointer. So can be used for either Oracle or MySQL
   db_connect => factory%create_connection()
   CALL db_connect%description()

   CALL factory%finalize()        ! Destroy the object

  END PROGRAM main

! { dg-final { cleanup-modules "factory_pattern" } }
