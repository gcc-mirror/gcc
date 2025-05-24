-- FXACC00.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- FOUNDATION DESCRIPTION:
--      This foundation consists of a tagged type definition and several
--      record extensions.  Objects of each type have also been declared 
--      and given initial values.
--      
--      Visual Description of Type Extensions:
--      
--                         type Ticket_Request
--                                  |
--                   _______________|_________________
--                  |                                 |
--                  |                                 |
--         type Subscriber_Request             type VIP_Request
--                                                    |
--                                                    |
--                                             type Last_Minute_Request
--      
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Ada.Calendar;

package FXACC00 is

   type    Location_Type      is (Backstage, Orchestra, Center, Back, Balcony);
   type    Quantity_Type      is range 1 .. 100;
   subtype Season_Ticket_Type is Positive range 1 .. 1750;
   type    VIP_Status_Type    is (Mayor, City_Council, Visitor);
   type    Donation_Type      is (To_Charity, To_Theatre, Personal);

   Show_Of_Appreciation : constant Boolean := True;

   type Ticket_Request is tagged
      record
         Location          : Location_Type;
         Number_Of_Tickets : Quantity_Type;
      end record;


   type Subscriber_Request is new Ticket_Request with
      record
         Subscription_Number : Season_Ticket_Type;
      end record;


   type VIP_Request is new Ticket_Request with
      record
         Rank : VIP_Status_Type;
      end record;


   type Last_Minute_Request (Special_Consideration : Boolean) 
     is new VIP_Request with
      record
         Time_of_Request : Ada.Calendar.Time;
         case Special_Consideration is
            when True  => Donation : Donation_Type;
            when False => null;
         end case;
      end record;


   -- Object Declarations.


   Box_Office_Request     : Ticket_Request := 
                              (Location          => Back, 
                               Number_Of_Tickets => 2);

   Summer_Subscription    : Subscriber_Request := 
                              (Ticket_Request'(Box_Office_Request) 
                               with Subscription_Number => 567);  

   Mayoral_Ticket_Request : VIP_Request := 
                              (Location          => Backstage,
                               Number_Of_Tickets => 6,
                               Rank              => Mayor);
                                            
   Late_Request           : Last_Minute_Request (Show_Of_Appreciation) :=
                              (Special_Consideration => Show_Of_Appreciation, 
                               Location              => Orchestra, 
                               Number_Of_Tickets     => 2, 
                               Rank                  => City_Council,
                               Time_Of_Request       => Ada.Calendar.Clock,
                               Donation              => To_Charity);


end FXACC00;
