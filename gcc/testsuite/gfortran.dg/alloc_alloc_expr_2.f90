! { dg-do compile }
! This tests the fix for PR29343, in which the valid ALLOCATE statement
! below triggered an error following the patch for PR20779 and PR20891.
!
! Contributed by Grigory Zagorodnev <grigory_zagorodnev@linux.intel.com>
!
        Subroutine ReadParameters (Album)
        Implicit NONE


        Type GalleryP
                Integer       :: NoOfEntries
                Character(80), Pointer :: FileName (:)
        End Type GalleryP


        Type(GalleryP), Intent(Out) :: Album
        Allocate (Album%FileName   (Album%NoOfEntries))
        end
