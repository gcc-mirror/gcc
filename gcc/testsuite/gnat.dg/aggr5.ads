        
package aggr5 is
   type Event is limited interface;
   type Event_Access is access all Event'Class;
   type Q_Action_Event is limited interface and Event;
   function Build (X : integer) return Event_Access;
end aggr5;
