with Opt106_Pkg2; use Opt106_Pkg2;

package body Opt106_Pkg1 is

  procedure Proc (Obj     : in out T;
                  Data    : Integer;
                  Last    : Boolean;
                  Stretch : Boolean) is

  begin
    if Stretch and then (Obj.Delayed /= 0 or else not Obj.Attach_Last) then
      raise Program_Error;
    end if;

    if Obj.Delayed /= 0 then
      Stop (Obj.Delayed, Obj.Before, Data, False);
    end if;

    if Last or (Obj.Delayed = 0 and not Stretch) then
      Stop (Data, Obj.Before, 0, Last);

      if Last then
        Obj.Initialized := False;
      else
        Obj.Next := 0;
        Obj.Before := Data;
      end if;

    else
      if Stretch then
        Obj.Next := 1;
      else
        Obj.Before := Obj.Delayed;
      end if;
      Obj.Delayed := Data;
    end if;
  end;

end Opt106_Pkg1;
