with Ada.Calendar;          use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Calendar;         use GNAT.Calendar;

package body Opt70_Pkg is

   type Enum is (None, Zero, Space);

   type Sec_Number is mod 2 ** 64;

   function Image (N : Sec_Number; Pad : Enum; Length : Natural) return String;

   function Image (N : Natural; Pad : Enum; Length: Natural) return String is
   begin
      return Image (Sec_Number (N), Pad, Length);
   end;

   function Image (N : Sec_Number; Pad : Enum; Length : Natural) return String is

      function Pad_Char return String is
      begin
         case Pad is
            when None  => return "";
            when Zero  => return "00";
            when Space => return "  ";
         end case;
      end;

      NI  : constant String := Sec_Number'Image (N);
      NIP : constant String := Pad_Char & NI (2 .. NI'Last);

   begin
      return NIP (NIP'Last - Length + 1 .. NIP'Last);
   end;

   function Image (Date : Ada.Calendar.Time; S : String) return String is
      Result     : Unbounded_String := Null_Unbounded_String;
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      case S (S'First + 1) is
         when 'S' => Result := Result & Image (Second, Zero, 2);
         when 'y' => Result := Result & Image (Sec_Number (1), Zero, 2);
         when others => null;
      end case;
      return To_String (Result);
   end;

end Opt70_Pkg;
