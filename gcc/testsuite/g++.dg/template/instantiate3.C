// { dg-do compile }
// Origin: Scott Snyder <snyder@fnal.gov>

// PR c++/7639
// ICE when accessing member with incomplete type.

class ACE_Null_Mutex;	// { dg-error "forward declaration" }

template <class TYPE>
struct ACE_Cleanup_Adapter
{
  TYPE &object ()
  { return object_; }
  TYPE object_;		// { dg-error "incomplete type" }
};

template class ACE_Cleanup_Adapter<ACE_Null_Mutex>; // { dg-message "required from here" }
