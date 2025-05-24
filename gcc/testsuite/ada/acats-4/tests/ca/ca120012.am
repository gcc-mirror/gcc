-- CA120012.AM
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
--      See CA120010.A.
--
--  TEST DESCRIPTION:
--      See CA120010.A.
--
--  SPECIAL REQUIREMENTS:
--      See CA120010.A.
--
--  TEST FILES:
--      This test consists of the following files:
--         CA120010.A
--         CA120011.A
--      -> CA120012.AM
--
--  CHANGE HISTORY:
--    11 Apr 2007 RLB Created test.
--    25 Apr 2007 RLB Split into separate files so that the various units
--                    can be added to the environment independently. Added
--                    special requirements to make it clear when the limited
--                    views need to be added to the environment.
--    18 Mar 2009 RLB Corrected the name of the main subprogram to be
--                    consistent with the naming conventions in the ACATS
--                    user's guide.
--!

package body CA12001_Win.Impl is

    procedure Open (Win : in out Low_Win_Type) is
    begin
       Win := (Width => 0, Height => 0);
       -- Would call the underlying system.
    end Open;

    procedure Close (Win : in out Low_Win_Type) is
    begin
       null;
       -- Would call the underlying system.
    end Close;

    procedure Set_Size (Win : in out Low_Win_Type;
                        Width  : in Window_Size;
                        Height : in Window_Size) is
    begin
       Win := (Width => Width, Height => Height);
       -- Would call the underlying system.
    end Set_Size;

    procedure Get_Size (Win : in Low_Win_Type;
                        Width  : out Window_Size;
                        Height : out Window_Size) is
    begin
       Width := Win.Width;
       Height := Win.Height;
       -- Would call the underlying system.
    end Get_Size;

end CA12001_Win.Impl;

---------------

with CA12001_Win.Impl;
with Ada.Unchecked_Deallocation;
package body CA12001_Win is
    -- A simple windowing package.

    procedure Free is new Ada.Unchecked_Deallocation (CA12001_Win.Impl.Low_Win_Type,
          Low_Win_Access);

    procedure Create (Window : in out Window_Type;
                      Id     : in Window_Id_Type;
                      Width  : in Window_Size;
                      Height : in Window_Size) is
       -- Create a new top-level window with the specified characteristics.
       -- Raises Not_Valid_Error if the Window has already been created.
    begin
       if Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       Window.Parent := null;
       Window.Child := null;
       Window.Next_Sibling := null;
       Window.Is_Created := True;
       Window.Id := Id;
       Window.Win_Obj := new CA12001_Win.Impl.Low_Win_Type;
       CA12001_Win.Impl.Open (Window.Win_Obj.all);
       CA12001_Win.Impl.Set_Size (Window.Win_Obj.all, Width, Height);
    end Create;

    procedure Create (Window : in out Window_Type;
                      Parent : in out Window_Type'Class;
                      Id     : in Window_Id_Type;
                      Width  : in Window_Size;
                      Height : in Window_Size) is
       -- Create a new child window with the specified characteristics.
       -- Raises Not_Valid_Error if the Window has already been created.
    begin
       if Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       if not Parent.Is_Created then
           raise Not_Valid_Error;
       end if;
       Window.Is_Created := True;
       Window.Parent := Parent'Unchecked_Access;
       Window.Child := null;
       if Parent.Child = null then
          Parent.Child := Window'Unchecked_Access;
          Window.Next_Sibling := null;
       else
          Window.Next_Sibling := Parent.Child;
          Parent.Child := Window'Unchecked_Access;
       end if;
       Window.Id := Id;
       Window.Win_Obj := new CA12001_Win.Impl.Low_Win_Type;
       CA12001_Win.Impl.Open (Window.Win_Obj.all);
       CA12001_Win.Impl.Set_Size (Window.Win_Obj.all, Width, Height);
    end Create;

    procedure Destroy (Window : in out Window_Type) is
       -- Destroy a window.
       -- Raises Not_Valid_Error if the Window has not been created, or
       -- if Window has any children that have not been destroyed.
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       elsif Window.Child /= null then
           raise Not_Valid_Error;
       end if;
       CA12001_Win.Impl.Close (Window.Win_Obj.all);
       Free (Window.Win_Obj);
       if Window.Parent /= null then
          -- Remove the window from the parent's list:
          if Window.Parent.Child = Window'Unchecked_Access then
             Window.Parent.Child := Window.Next_Sibling;
          else
             declare
                Cursor : Any_Window_Access_Type := Window.Parent.Child;
             begin
                loop
                    if Cursor.Next_Sibling = Window'Unchecked_Access then
                       Cursor.Next_Sibling := Window.Next_Sibling;
                       exit;
                    end if;
                    Cursor := Cursor.Next_Sibling;
                end loop;
             end;
          end if;
       end if;
       Window.Is_Created := False;
    end Destroy;

    function Id (Window : in Window_Type) return Window_Id_Type is
       -- Returns the Id of a window.
       -- Raises Not_Valid_Error if the Window has not been created.
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       return Window.Id;
    end Id;

    function Height (Window : in Window_Type) return Window_Size is
       -- Returns the Height of a window.
       -- Raises Not_Valid_Error if the Window has not been created.
       Temp_Width, Temp_Height : Window_Size;
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       CA12001_Win.Impl.Get_Size (Window.Win_Obj.all,
                                  Width => Temp_Width,
                                  Height => Temp_Height);
       return Temp_Height;
    end Height;

    function Width (Window : in Window_Type) return Window_Size is
       -- Returns the Width of a window.
       -- Raises Not_Valid_Error if the Window has not been created.
       Temp_Width, Temp_Height : Window_Size;
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       CA12001_Win.Impl.Get_Size (Window.Win_Obj.all,
                                  Width => Temp_Width,
                                  Height => Temp_Height);
       return Temp_Width;
    end Width;

    procedure Resize (Window : in Window_Type;
                      Width  : in Window_Size;
                      Height : in Window_Size) is
       -- Change the size of a window.
       -- Raises Not_Valid_Error if the Window has not been created.
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       CA12001_Win.Impl.Set_Size (Window.Win_Obj.all,
                                  Width => Width,
                                  Height => Height);
    end Resize;

    function Parent (Window : in Window_Type) return Any_Window_Access_Type is
       -- Returns an access to the parent of this window. Returns null if
       -- this is a top-level window.
       -- Raises Not_Valid_Error if the Window has not been created.
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       return Window.Parent;
    end Parent;

    function Child (Window : in Window_Type) return Any_Window_Access_Type is
       -- Returns an access to a child of this window. Returns null if
       -- this window has no children.
       -- Raises Not_Valid_Error if the Window has not been created.
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       return Window.Child;
    end Child;

    function Sibling (Window : in Window_Type) return Any_Window_Access_Type is
       -- Returns an access to a sibling of this window. Returns null if
       -- this window has no siblings.
       -- Raises Not_Valid_Error if the Window has not been created.
    begin
       if not Window.Is_Created then
           raise Not_Valid_Error;
       end if;
       return Window.Next_Sibling;
    end Sibling;

end CA12001_Win;

---------------

with Report;      use Report;
with CA12001_Win; use CA12001_Win;
with Ada.Exceptions;
procedure CA120012 is
    Top_Win    : Window_Type;
    Child1_Win : aliased Window_Type;
    Child2_Win : Window_Type;
    GrandChild1_Win : aliased Window_Type;
    GrandChild2A_Win : aliased Window_Type;
    GrandChild2B_Win : aliased Window_Type;
    Not_Used_Win : Window_Type;

    procedure TC_Check (Window : Window_Type'Class;
                        Expected_Id : Window_Id_Type;
                        Expected_Width : Window_Size;
                        Expected_Height : Window_Size;
                        Check_Name : String) is
    begin
       if Id (Window) /= Expected_Id then
          Failed ("Wrong Id for " & Check_Name & "; saw: " & Id (Window) &
          "; expected: " & Expected_Id);
       end if;
       if Width (Window) /= Ident_Int (Expected_Width) then
          Failed ("Wrong Id for " & Check_Name & "; saw:" &
             Window_Size'Image (Width (Window)) & "; expected:" &
             Window_Size'Image (Expected_Width));
       end if;
       if Height (Window) /= Ident_Int (Expected_Height) then
          Failed ("Wrong Id for " & Check_Name & "; saw:" &
             Window_Size'Image (Height (Window)) & "; expected:" &
             Window_Size'Image (Expected_Height));
       end if;
    end TC_Check;

begin
    Test ("CA120012","Check that a limited private with clause for a " &
                     "private child of a package P can be given on the " &
                     "declaration of P");

    -- Create the windows:
    Create (Top_Win, Id => 'F', Width => 120, Height => 80);
    Create (Child1_Win, Parent => Top_Win, Id => 'J', Width => 80, Height => 50);
    Create (Child2_Win, Parent => Top_Win, Id => 'W', Width => 80, Height => 25);
    Create (GrandChild1_Win, Parent => Child1_Win, Id => 'R', Width => 40, Height => 10);
    Create (GrandChild2A_Win, Parent => Child2_Win, Id => 'D', Width => 40, Height => 15);
    Create (GrandChild2B_Win, Parent => Child2_Win, Id => 'S', Width => 35, Height => 12);

    -- Test the attributes of the windows:
    if Id (Top_Win) /= 'F' then
       Failed ("Wrong Id for Top_Win: " & Id (Top_Win));
    end if;
    if Width (Child1_Win) /= Ident_Int (80) then
       Failed ("Wrong Width for Child 1: " &
          Window_Size'Image (Width (Child1_Win)));
    end if;
    if Height (Child2_Win) /= Ident_Int (25) then
       Failed ("Wrong Height for Child 2: " &
          Window_Size'Image (Height (Child2_Win)));
    end if;

    -- Check all of the windows:
    TC_Check (Top_Win, Expected_Id => 'F',
	      Expected_Width => 120, Expected_Height => 80,
              Check_Name => "Top window");
    TC_Check (Child1_Win, Expected_Id => 'J',
              Expected_Width => 80, Expected_Height => 50,
              Check_Name => "Child 1");
    TC_Check (Child2_Win, Expected_Id => 'W',
              Expected_Width => 80, Expected_Height => 25,
              Check_Name => "Child 2");
    TC_Check (GrandChild1_Win, Expected_Id => 'R',
              Expected_Width => 40, Expected_Height => 10,
              Check_Name => "Grandchild 1");
    TC_Check (GrandChild2A_Win, Expected_Id => 'D',
              Expected_Width => 40, Expected_Height => 15,
              Check_Name => "Grandchild 2A");
    TC_Check (GrandChild2B_Win, Expected_Id => 'S',
              Expected_Width => 35, Expected_Height => 12,
              Check_Name => "Grandchild 2B");


    if Parent (Top_Win) /= null then
       Failed ("Top window has parent");
    end if;
    if Parent (GrandChild1_Win) /= Child1_Win'Unchecked_Access then
       if Parent (GrandChild1_Win) = null then
           Failed ("Grandchild has no parent");
       else
           Failed ("Grandchild has wrong parent " &
              Id (Parent (GrandChild1_Win).all));
       end if;
    end if;
    if Child (Child1_Win) /= GrandChild1_Win'Unchecked_Access then
       if Child (Child1_Win) = null then
           Failed ("Child 1 has no child");
       else
           Failed ("Child 1 has wrong child " & Id (Child (Child1_Win).all));
       end if;
    end if;
    if Sibling (GrandChild2A_Win) /= null and then
       Sibling (GrandChild2A_Win) /= GrandChild2B_Win'Unchecked_Access then
       Failed ("Grandchild 2A has wrong sibling " &
          Id (Sibling (GrandChild2A_Win).all));
    end if;
    if Sibling (GrandChild2A_Win) = null and then
       Sibling (GrandChild2B_Win) = null then
       Failed ("Neither grandchild 2 has a sibling");
    end if;
    if Width (Child (Top_Win).all) /= Ident_Int (80) then
       -- Both children have the same width:
       Failed ("Wrong Width for child of Top_Win: " &
          Window_Size'Image (Width (Child (Top_Win).all)));
    end if;
    Resize (Child (Child1_Win).all, Width => 20, Height => 30);
    TC_Check (GrandChild1_Win, Expected_Id => 'R',
              Expected_Width => 20, Expected_Height => 30,
              Check_Name => "Resize window");

    -- Try a few error cases:
    begin
       if Width (Not_Used_Win) = Ident_Int (10) then
          Comment ("Unusual");
       end if;
       Failed ("Use of Width on unused window not detected");
    exception
       when Not_Valid_Error => null; -- Expected.
    end;
    begin
       if Child (Not_Used_Win) = null then
          Comment ("Unusual");
       end if;
       Failed ("Use of Child on unused window not detected");
    exception
       when Not_Valid_Error => null; -- Expected.
    end;
    begin
       Create (Grandchild1_Win, Parent => Child1_Win, Id => 'R',
               Width => 40, Height => 10);
       Failed ("Creating already created window not detected");
    exception
       when Not_Valid_Error => null; -- Expected.
    end;

    -- Destroy windows:
    Destroy (GrandChild1_Win);
    Destroy (GrandChild2A_Win);
    Destroy (GrandChild2B_Win);
    Destroy (Child2_Win);
    Destroy (Child1_Win);
    Destroy (Top_Win);

    Result;
exception
    when Oops:others =>
        Failed ("Unexpected exception: " & Ada.Exceptions.Exception_Name (Oops));
        Comment ("Exception Info: " & Ada.Exceptions.Exception_Information (Oops));
end CA120012;

