--  { dg-do compile }

package limited1 is
   pragma Pure;
   
   type Buffer is limited interface;
   type Synchronous_Buffer_Type is synchronized interface and Buffer;
   type Client_Buffer_Type is new Synchronous_Buffer_Type with private;

private
   type Client_Buffer_Type is new Synchronous_Buffer_Type with null record;
end limited1;
