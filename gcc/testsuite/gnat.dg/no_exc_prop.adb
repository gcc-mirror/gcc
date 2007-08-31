--  { dg-do compile }
--  { dg-options "-gnatwa" }

package body no_exc_prop is
   protected body Simple_Barrier is
      entry Wait when Signaled is
      begin
        Signaled := False;
      end Wait;
      procedure Signal is
      begin
        Signaled := True;
      end Signal;
   end Simple_Barrier;
end no_exc_prop;
