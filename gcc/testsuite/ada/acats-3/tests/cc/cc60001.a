-- CC60001.A
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
--
-- OBJECTIVE:
--      Check that calls on formal abstract subprograms dispatch as expected.
--
-- TEST DESCRIPTION:
--      We declare a hierarchy of tagged types (the outline is borrowed from
--      foundation F392C00), declare a generic package that operates on it,
--      instantiate the generic, and then check that calls to the instance
--      properly dispatch.
--
--      A generic package like CC60001_2 below functions something like an
--      after-the-fact interface, as any tagged type with the appropriate
--      operations can instantiate it; the type is not restricted to a
--      single hierarchy.
--
-- CHANGE HISTORY:
--      29 May 2015   RLB   Created test.

package CC60001_1 is  -- Switches

  type Toggle is tagged private;    ---------------------------------- Toggle

  function Create return Toggle;
  procedure Flip   (It : in out Toggle);
  function Is_On   (It : Toggle) return Boolean;

  type Dimmer is new Toggle with private;    ------------------------- Dimmer

  type Luminance is range 0..100;

  function  Create return Dimmer;
  overriding
  procedure Flip    (It : in out Dimmer);
  procedure Brighten(It : in out Dimmer;
                      By : in Luminance := 10);
  procedure Dim     (It : in out Dimmer;
                      By : in Luminance := 10);
  function Intensity(It : Dimmer) return Luminance;
  --function Is_On   (It : Dimmer) return Boolean; -- Inherited.

  type Auto_Dimmer is new Dimmer with private;    --------------- Auto_Dimmer

  function  Create return Auto_Dimmer;
  overriding
  procedure Flip      (It: in out Auto_Dimmer);
  procedure Set_Auto  (It: in out Auto_Dimmer);
  procedure Clear_Auto(It: in out Auto_Dimmer);
  -- procedure Set_Manual(It: in out Auto_Dimmer) renames Clear_Auto;
  procedure Set_Cutin (It: in out Auto_Dimmer; Lumens: in Luminance);
  procedure Set_Cutout(It: in out Auto_Dimmer; Lumens: in Luminance);

  function Auto            (It: Auto_Dimmer) return Boolean;
  function Cutout_Threshold(It: Auto_Dimmer) return Luminance;
  function Cutin_Threshold (It: Auto_Dimmer) return Luminance;
  --function Is_On   (It : Auto_Dimmer) return Boolean; -- Inherited.

  type Duo is new Toggle with private;    ---------------------------- Duo
     -- Two switches working together.

  function Create return Duo;
  --procedure Flip   (It : in out Duo); -- Inherited.
  procedure Flip_1 (It : in out Duo) renames Flip;
  procedure Flip_2   (It : in out Duo);
  overriding
  function Is_On   (It : Duo) return Boolean;


  function TC_CW_TI( Key : Character) return Toggle'Class;

private

  type Toggle is tagged record
    On : Boolean := False;
  end record;

  type Dimmer is new Toggle with record
    Intensity : Luminance := 100;
  end record;

  type Auto_Dimmer is new Dimmer with record
    Cutout_Threshold : Luminance := 60;
    Cutin_Threshold  : Luminance := 40;
    Auto_Engaged     : Boolean := False;
  end record;

  type Duo is new Toggle with record
    On_2 : Boolean := False; -- Second physical switch.
      -- Note: If both are in the "On" position, the switch is off.
  end record;

end CC60001_1;

with TCTouch;
package body CC60001_1 is

  function Create return Toggle is
  begin
    TCTouch.Touch ('1');  ------------------------------------------------ 1
    return Toggle'( On => True);
  end Create;

  function Create return Dimmer is
  begin
    TCTouch.Touch ('2');  ------------------------------------------------ 2
    return Dimmer'( On => True, Intensity => 75);
  end Create;

  function Create return Auto_Dimmer is
  begin
    TCTouch.Touch ('3');  ------------------------------------------------ 3
    return  Auto_Dimmer'( On => True, Intensity => 25,
                          Cutout_Threshold | Cutin_Threshold => 50,
                          Auto_Engaged => True);
  end Create;

  function Create return Duo is
  begin
    TCTouch.Touch ('4');  ------------------------------------------------ 4
    return Duo'( On => True, On_2 => True);
  end Create;

  procedure Flip   (It : in out Toggle) is
  begin
    TCTouch.Touch ('A');  ------------------------------------------------ A
    It.On := not It.On;
  end Flip;

  function Is_On   (It : Toggle) return Boolean is
  begin
    TCTouch.Touch ('B');  ------------------------------------------------ B
    return It.On;
  end Is_On;

  procedure Brighten(It : in out Dimmer;
                      By : in Luminance := 10) is
  begin
    TCTouch.Touch ('D');  ------------------------------------------------ D
    if (It.Intensity+By) <= Luminance'Last then
      It.Intensity := It.Intensity+By;
    else
      It.Intensity := Luminance'Last;
    end if;
  end Brighten;

  procedure Dim     (It : in out Dimmer;
                      By : in Luminance := 10) is
  begin
    TCTouch.Touch ('E');  ------------------------------------------------ E
    if (It.Intensity-By) >= Luminance'First then
      It.Intensity := It.Intensity-By;
    else
      It.Intensity := Luminance'First;
    end if;
  end Dim;

  function Intensity(It : Dimmer) return Luminance is
  begin
    TCTouch.Touch ('F');  ------------------------------------------------ F
    if Is_On(It) then ---------------------------------------------------- B
      return It.Intensity;
    else
      return Luminance'First;
    end if;
  end Intensity;

  procedure Flip    (It : in out Dimmer) is
  begin
    TCTouch.Touch ('G');  ------------------------------------------------ G
    if Is_On(It) and (It.Intensity < 50) then ---------------------------- B
      It.Intensity := Luminance'Last - It.Intensity;
    else
      Flip( Toggle(It)); ------------------------------------------------- A
    end if;
  end Flip;

  procedure Set_Auto  (It: in out Auto_Dimmer) is
  begin
    TCTouch.Touch ('H');  ------------------------------------------------ H
    It.Auto_Engaged := True;
  end Set_Auto;

  procedure Clear_Auto(It: in out Auto_Dimmer) is
  begin
    TCTouch.Touch ('I');  ------------------------------------------------ I
    It.Auto_Engaged := False;
  end Clear_Auto;

  function  Auto           (It: Auto_Dimmer) return Boolean is
  begin
    TCTouch.Touch ('J');  ------------------------------------------------ J
    return It.Auto_Engaged;
  end Auto;

  procedure Flip     (It: in out Auto_Dimmer) is
  begin
    TCTouch.Touch ('K');  ------------------------------------------------ K
    if It.Auto_Engaged then
      if not Is_On(It) then ---------------------------------------------- B
       Flip( Dimmer(It));   ---------------------------------------------- G
      else
       It.Auto_Engaged := False;
      end if;
    else
      Flip( Dimmer(It));    ---------------------------------------------- G
    end if;
  end Flip;

  procedure Set_Cutin (It : in out Auto_Dimmer;
                        Lumens : in Luminance) is
  begin
    TCTouch.Touch ('L');  ------------------------------------------------ L
    It.Cutin_Threshold := Lumens;
  end Set_Cutin;

  procedure Set_Cutout(It : in out Auto_Dimmer;
                        Lumens : in Luminance) is
  begin
    TCTouch.Touch ('M');  ------------------------------------------------ M
    It.Cutout_Threshold := Lumens;
  end Set_Cutout;

  function Cutout_Threshold(It : Auto_Dimmer) return Luminance is
  begin
    TCTouch.Touch ('N');  ------------------------------------------------ N
    return It.Cutout_Threshold;
  end Cutout_Threshold;

  function Cutin_Threshold (It : Auto_Dimmer) return Luminance is
  begin
    TCTouch.Touch ('O');  ------------------------------------------------ O
    return It.Cutin_Threshold;
  end Cutin_Threshold;

  procedure Flip_2   (It : in out Duo) is
  begin
    TCTouch.Touch ('R');  ------------------------------------------------ R
    It.On_2 := not It.On_2;
  end Flip_2;

  function Is_On   (It : Duo) return Boolean is
  begin
    TCTouch.Touch ('S');  ------------------------------------------------ S
    return It.On xor It.On_2;
  end Is_On;


  function TC_CW_TI( Key : Character) return Toggle'Class is
  begin
    TCTouch.Touch ('W');  ------------------------------------------------ W
    case Key is
      when 'T' | 't' => return Toggle'( On => True);
      when 'D' | 'd' => return Dimmer'( On => True, Intensity => 75);
      when 'A' | 'a' => return Auto_Dimmer'( On => True, Intensity => 25,
                                 Cutout_Threshold | Cutin_Threshold => 50,
                                 Auto_Engaged => True);
      when '2' => return Duo'( On => True, On_2 => True);
      when others => null;
    end case;
  end TC_CW_TI;

end CC60001_1;

generic
   type Switch is tagged limited private;
   with procedure Flip   (It : in out Switch) is abstract <>;
   with function Is_On   (It : Switch) return Boolean is abstract <>;
package CC60001_2 is -- Switch operations.

   procedure Force_On (It : in out Switch'Class);

   procedure Force_Off (It : in out Switch'Class);

end CC60001_2;

package body CC60001_2 is -- Switch operations.

   procedure Force_On (It : in out Switch'Class) is
   begin
      if Is_On (It) then
         null; -- Already on, nothing to do.
      else
         Flip (It);
      end if;
   end Force_On;

   procedure Force_Off (It : in out Switch'Class) is
   begin
      if Is_On (It) then
         Flip (It);
      else
         null; -- Already off, nothing to do.
      end if;
   end Force_Off;

end CC60001_2;

with Report;
with CC60001_1; use CC60001_1;
with CC60001_2;
with TcTouch;
procedure CC60001 is

begin

   Report.Test ("CC60001",
                 "Check that calls on formal abstract subprograms dispatch " &
                 "as expected");

   -- Try various objects descended from Toggle:
   declare
      My_Auto_Dim : Auto_Dimmer := Create; ------------------------------ 3
      My_Duo : Duo := Create;              ------------------------------ 4
      My_Toggle : Toggle := Create;        ------------------------------ 1
      My_Class : Toggle'Class := TC_CW_TI ('2'); ------------------------ W

      package Inst is new CC60001_2 (Toggle); -- Subprograms are found via <>

   begin
      if not Is_On (My_Auto_Dim) then ----------------------------------- B
         Report.Failed ("Wrong initial state (1A)");
      end if;
      if Is_On (My_Duo) then -------------------------------------------- S
         Report.Failed ("Wrong initial state (1B)");
      end if;
      if not Is_On (My_Toggle) then ------------------------------------- B
         Report.Failed ("Wrong initial state (1C)");
      end if;
      if Is_On (My_Class) then ------------------------------------------ S
         Report.Failed ("Wrong initial state (1D)");
      end if;

      TcTouch.Validate (Expected => "341WBSBS",
                        Message => "Wrong routines initially called (1E)");

      Inst.Force_On (My_Auto_Dim); -------------------------------------- B

      Inst.Force_Off (My_Toggle); -------------------------------------- BA

      Inst.Force_On (My_Duo); ------------------------------------------ SA

      Inst.Force_Off (My_Class); ---------------------------------------- S

      if not Is_On (My_Auto_Dim) then ----------------------------------- B
         Report.Failed ("Wrong state (1F)");
      end if;
      if not Is_On (My_Duo) then ---------------------------------------- S
         Report.Failed ("Wrong state (1G)");
      end if;
      if Is_On (My_Toggle) then ----------------------------------------- B
         Report.Failed ("Wrong state (1H)");
      end if;
      if Is_On (My_Class) then ------------------------------------------ S
         Report.Failed ("Wrong state (1I)");
      end if;

      TcTouch.Validate (Expected => "BBASASBSBS",
                        Message => "Wrong routines called (1J)");

      Flip_2 (My_Duo); -------------------------------------------------- R

      Inst.Force_On (My_Duo); ------------------------------------------ SA

      if not Is_On (My_Duo) then ---------------------------------------- S
         Report.Failed ("Wrong state (1K)");
      end if;

      if not Auto (My_Auto_Dim) then ------------------------------------ J
         Report.Failed ("Wrong state (1L)");
      end if;

      Inst.Force_Off (My_Auto_Dim); ----------------------------------- BKB
         -- Changes Auto, but doesn't turn it off.

      if Auto (My_Auto_Dim) then ---------------------------------------- J
         Report.Failed ("Wrong state (1M)");
      end if;

      if not Is_On (My_Auto_Dim) then ----------------------------------- B
         Report.Failed ("Wrong state (1N)");
      end if;

      TcTouch.Validate (Expected => "RSASJBKBJB",
                        Message => "Wrong routines called (1O)");

   end;


   -- Try a type unrelated to Toggle:
   declare
      package Inner is
         type Clicker is tagged record
            Is_Set : Boolean;
         end record;

         function Create return Clicker;
         procedure Invert  (It : in out Clicker);
         function Set    (It : Clicker) return Boolean;
      end Inner;
      use Inner;

      package Inst2 is new CC60001_2 (Clicker, Flip => Invert, Is_On => Set);

      package body Inner is
         function Create return Clicker is
         begin
            TCTouch.Touch ('9');  ---------------------------------------- 9
            return Clicker'( Is_Set => True);
         end Create;

         procedure Invert  (It : in out Clicker) is
         begin
            TCTouch.Touch ('a');  ---------------------------------------- a
            It.Is_Set := not It.Is_Set;
         end Invert;

         function Set    (It : Clicker) return Boolean is
         begin
            TCTouch.Touch ('b');  ---------------------------------------- b
            return It.Is_Set;
         end Set;

      end Inner;

      My_Clicker : Clicker := Create; ------------------------------------ 9

   begin
      Inst2.Force_Off (My_Clicker); ------------------------------------- ba

      Inst2.Force_Off (My_Clicker); -------------------------------------- b

      if My_Clicker.Is_Set then
         Report.Failed ("Wrong state (2A)");
      end if;

      Inst2.Force_On (My_Clicker); -------------------------------------- ba

      if not My_Clicker.Is_Set then
         Report.Failed ("Wrong state (2B)");
      end if;

      TcTouch.Validate (Expected => "9babba",
                        Message => "Wrong routines called (2C)");


   end;

   Report.Result;

end CC60001;
