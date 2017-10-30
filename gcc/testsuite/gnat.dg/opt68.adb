-- { dg-do compile }
-- { dg-options "-O3" }

with Ada.Unchecked_Deallocation;

package body Opt68 is

  procedure Free
    is new Ada.Unchecked_Deallocation (Queue_Element, A_Queue_Element);

  procedure Copy (dest : in out Queue; src : Queue) is
    d, s, pd, ps, t : A_Queue_Element;
  begin
    if src.sz /= 0 then
      d := dest.front;
      s := src.front;
      while d /= null and s /= null loop
        d.value := s.value;
        pd := d;
        ps := s;
        d  := d.next;
        s  := s.next;
      end loop;
      if src.sz = dest.sz then
        return;
      elsif s = null then
        while d /= null loop
          t := d.next;
          Free (d);
          d := t;
        end loop;
        dest.back      := pd;
        dest.back.next := null;
      else
        if pd = null then
          dest.front       := new Queue_Element;
          dest.front.value := s.value;
          s                := s.next;
          pd               := dest.front;
        end if;
        while s /= null loop
          pd.next       := new Queue_Element;
          pd.next.value := s.value;
          pd            := pd.next;
          s             := s.next;
        end loop;
        dest.back := pd;
      end if;
      dest.sz := src.sz;
    end if;
  end;

end Opt68;
