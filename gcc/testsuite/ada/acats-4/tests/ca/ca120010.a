-- CA120010.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--  OBJECTIVE:
--     Check that a limited private with clause for a private child of
--     a package P can be given on the declaration of P.
--
--  TEST DESCRIPTION:
--
--     This test is intended to illustrate a likely way that this feature
--     could be used in practice.
--
--     We declare a parent package CA12001_Win which defines a reusable
--     window abstraction, and a private child CA12001_Win.Impl which
--     defines the low-level implemention of that abstraction for a particular
--     target. The main subprogram CA12001 exercises the window abstraction.
--
--     This organization makes it impossible for clients to "cheat" and access
--     the implementation package directly, while making it possible for that
--     package to be shared with all of the children of the root package.
--
--     This test is based on the initial design of the Claw windowing library.
--     (That design was heavily compromised because of the lack of limited
--     with and private with in Ada 95; most of the low-level implementation
--     was moved into the private parts of the various packages.)
--
--  SPECIAL REQUIREMENTS:
--      To build this test:
--         1) Do the steps required to add the limited view of the file
--            CA120011 to the compilation environment. (Careful: the
--            compilation of the normal view of the file CA120011 should
--            not be done at this time.)
--         2) Compile the file CA120010 (and add the results to the
--            environment).
--         3) Compile the file CA120011 (and add the results to the
--            environment).
--         4) Compile the file CA120012 (and add the results to the
--            environment).
--         5) Build an executable image and run it.
--
--  TEST FILES:
--      This test consists of the following files:
--      -> CA120010.A
--         CA120011.A
--         CA120012.AM
--
--  CHANGE HISTORY:
--    11 Apr 2007 RLB Created test.
--    25 Apr 2007 RLB Split into separate files so that the various units
--                    can be added to the environment independently. Added
--                    special requirements to make it clear when the limited
--                    views need to be added to the environment.
--
--!

limited private with CA12001_Win.Impl;
package CA12001_Win is
    -- A simple windowing package.

    type Window_Type is tagged limited private;

    type Any_Window_Access_Type is access all Window_Type'Class;

    subtype Window_Id_Type is Character; -- A stand-in for a real type.

    subtype Window_Size is Natural; -- A stand-in for a real type.

    Not_Valid_Error : exception;

    procedure Create (Window : in out Window_Type;
                      Id     : in Window_Id_Type;
                      Width  : in Window_Size;
                      Height : in Window_Size);
       -- Create a new top-level window with the specified characteristics.
       -- Raises Not_Valid_Error if the Window has already been created.

    procedure Create (Window : in out Window_Type;
                      Parent : in out Window_Type'Class;
                      Id     : in Window_Id_Type;
                      Width  : in Window_Size;
                      Height : in Window_Size);
       -- Create a new child window with the specified characteristics.
       -- Raises Not_Valid_Error if the Window has already been created,
       -- or if the Parent window has not been created.

    procedure Destroy (Window : in out Window_Type);
       -- Destroy a window.
       -- Raises Not_Valid_Error if the Window has not been created, or
       -- if Window has any children that have not been destroyed.

    function Id (Window : in Window_Type) return Window_Id_Type;
       -- Returns the Id of a window.
       -- Raises Not_Valid_Error if the Window has not been created.

    function Height (Window : in Window_Type) return Window_Size;
       -- Returns the Height of a window.
       -- Raises Not_Valid_Error if the Window has not been created.

    function Width (Window : in Window_Type) return Window_Size;
       -- Returns the Width of a window.
       -- Raises Not_Valid_Error if the Window has not been created.

    procedure Resize (Window : in Window_Type;
                      Width  : in Window_Size;
                      Height : in Window_Size);
       -- Change the size of a window.
       -- Raises Not_Valid_Error if the Window has not been created.

    function Parent (Window : in Window_Type) return Any_Window_Access_Type;
       -- Returns an access to the parent of this window. Returns null if
       -- this is a top-level window.
       -- Raises Not_Valid_Error if the Window has not been created.

    function Child (Window : in Window_Type) return Any_Window_Access_Type;
       -- Returns an access to a child of this window. Returns null if
       -- this window has no children.
       -- Raises Not_Valid_Error if the Window has not been created.

    function Sibling (Window : in Window_Type) return Any_Window_Access_Type;
       -- Returns an access to a sibling of this window. Returns null if
       -- this window has no siblings.
       -- Raises Not_Valid_Error if the Window has not been created.

private
    type Low_Win_Access is access CA12001_Win.Impl.Low_Win_Type;
    type Window_Type is tagged limited record
        Is_Created : Boolean := False;
        Id : Window_Id_Type := ' ';
        Win_Obj : Low_Win_Access := null;
            -- Size information is accessed from the low-level object.
        -- Relationship pointers: (In a real package, we'd use controlled
        -- types to ensure that these are cleaned up.)
        Parent : Any_Window_Access_Type := null;
        Next_Sibling : Any_Window_Access_Type := null;
        Child : Any_Window_Access_Type := null;
        -- ... other components here.
    end record;
end CA12001_Win;
