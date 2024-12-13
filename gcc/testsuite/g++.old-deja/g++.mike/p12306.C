// { dg-do run  }
// prms-id: 12306

void *ptr1, *ptr2;
int fail = 0;

extern "C" int printf(const char *, ...);

class RWSlist { };

class RWSlistIterator {
public:
  RWSlist *slist;
  RWSlistIterator(RWSlist& s) { } 
  void toLast() {
    if (ptr1 != (RWSlistIterator*)this)
      fail = 5;
    if (ptr2 != &(*this).slist)
      fail = 6;

    if (0) printf("at %x %x\n", (RWSlistIterator*)this, &(*this).slist);
  }
};

class RWCollectable {
};

class RWSlistCollectables : public RWSlist {
public:	 
  RWSlistCollectables() { }
  RWSlistCollectables(RWCollectable* a) { }
};

class RWIterator { };	  

class RWSlistCollectablesIterator : public RWIterator, public RWSlistIterator {
public:
  RWSlistCollectablesIterator(RWSlistCollectables& s) : RWSlistIterator(s) { } 
};

class Sim_Event_Manager {
public:
  RWSlistCollectables scheduled_events_;
  RWSlistCollectablesIterator last_posted_event_position_;
  Sim_Event_Manager();
  void post_event();
};

Sim_Event_Manager::Sim_Event_Manager ()
  :last_posted_event_position_(scheduled_events_)
{
}

void Sim_Event_Manager::post_event () {
  ptr1 = (RWSlistIterator*)&last_posted_event_position_;
  ptr2 = &((RWSlistIterator*)&last_posted_event_position_)->slist;
  if (0) printf("at %x %x\n", (RWSlistIterator*)&last_posted_event_position_,
		&((RWSlistIterator*)&last_posted_event_position_)->slist);
  if (ptr1 != (RWSlistIterator*)&last_posted_event_position_)
    fail = 1;
  if (ptr2 != &((RWSlistIterator&)last_posted_event_position_).slist)
    fail = 2;
  if (0) printf("at %x ?%x\n", (RWSlistIterator*)&last_posted_event_position_,
		&((RWSlistIterator&)last_posted_event_position_).slist);
  if (ptr1 != (RWSlistIterator*)&last_posted_event_position_)
    fail = 3;
  if (ptr2 != &((RWSlistIterator&)last_posted_event_position_).slist)
    fail = 4;
  last_posted_event_position_.toLast();
}

int main(int argc, char **argv) {
  Sim_Event_Manager foo;
  foo.post_event();
  return fail;
}
