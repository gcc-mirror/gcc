-- { dg-options "-gnatws" }

package body test_prio_p is
   protected body Protected_Queue_T is
      entry Seize when True is begin null; end;
   end Protected_Queue_T;
end test_prio_p;
