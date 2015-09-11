-- { dg-do compile }

procedure Entry_Queues3 is

  generic
    type Large_Range is range <>;
  package Queue is
  end;

  package body Queue is
    task T is
      entry E(Large_Range);
    end T ;
 
    task body T is
      begin
        accept E(Large_Range'First) do
          null;
        end e ; 
      end T ;
  end Queue;
 
  type Large_Range is range 0 .. Long_Integer'Last;

  package My_Queue is new Queue(Large_Range); -- { dg-warning "warning" }

begin
  null;
end;
