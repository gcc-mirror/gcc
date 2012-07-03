-- { dg-do compile }
-- { dg-options "-gnat2012" }

function Recursive_Call (File : String; Status : out Boolean) return Boolean is
begin
  if File /= "/dev/null" then
    return Recursive_Call ("/dev/null", Status);
  end if;
  return False;
end;
