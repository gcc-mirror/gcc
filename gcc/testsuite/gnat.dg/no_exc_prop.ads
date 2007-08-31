pragma Restrictions (No_Exception_Propagation);
package no_exc_prop is
   protected Simple_Barrier is
      entry Wait;
      procedure Signal;
   private
         Signaled : Boolean := False;
   end Simple_Barrier;
end no_exc_prop;
