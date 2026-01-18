! { dg-do compile }
   module foo
      implicit none
      integer aa
   end module foo
   !
   ! AA made available by USE association in bar2, and imported into
   ! the interface.  This compiles with gfortran and flang21.
   !
   module bar2
      use foo
      implicit none
      interface
         subroutine bah
            import aa
         end subroutine bah
      end interface
   end module bar2
   !
   ! AA made available by USE association within bah, and then an
   ! import statement is invalid as is in the current scoping unit.
   !
   ! gfortran -c vv.f90
   ! vv.f90:40:17:
   !
   !   40 |          import aa
   !      |                 1
   ! Error: Cannot IMPORT 'aa' from host scoping unit at (1)...
   !
   ! flang21 -c vv.f90
   ! error: Semantic errors in vv.f90
   ! ./vv.f90:40:17: error: 'a' not found in host scope
   !           import aa
   !                  ^^
   module bar3
      implicit none
      interface
         subroutine bah
            use foo
            import aa      ! { dg-error "in the local scope" }
         end subroutine bah
      end interface
   end module bar3
   !
   ! AA made available by USE association within foo and bah.  The
   ! import statement should be invalid as AA is  the current scoping
   ! unit.
   !
   ! F2023:C8105 Within an interface body, an entity that is accessed
   ! by host association shall be accessible by host or use association
   ! within the host scoping unit, or explicitly declared prior to the
   ! interface body.
   !
   ! flang21 compiles this example.
   ! gfortran has an ICE.
   !
   module bar4
      use foo
      implicit none
      interface
         subroutine bah
            ! This brings aa into the interface, and should be an error.
            use foo
            import aa      ! { dg-error "in the local scope" }
         end subroutine bah
      end interface
   end module bar4
