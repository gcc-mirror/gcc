// { dg-do compile }

namespace std
{
  template < class _Elem > struct char_traits
  {
  };
  struct _Container_base
  {
  };
    template < class _Ty > struct _Allocator_base
  {
  };
    template < class _Ty > class allocator:public _Allocator_base < _Ty >
  {
  };
  class _String_base:public _Container_base
  {
  };
template < class _Ty, class _Alloc > class _String_val:public _String_base
  {
  };
template < class _Elem, class _Traits, class _Ax > class basic_string:public _String_val < _Elem,
    _Ax
    >
  {
  public:typedef basic_string < _Elem, _Traits, _Ax > _Myt;
    typedef _String_val < _Elem, _Ax > _Mybase;
    basic_string (const _Elem * _Ptr):_Mybase ()
    {
    }
  };
  typedef basic_string < char, char_traits < char >,
    allocator < char > >string;
}


namespace google
{
  namespace protobuf
  {
    namespace internal
    {
      template < class C > class scoped_ptr
      {
      public:typedef C element_type;
      explicit scoped_ptr (C * p = __null):ptr_ (p)
	{
	}
	 ~scoped_ptr ()
	{
	  delete ptr_;
	}
	C *get () const
	{
	  return ptr_;
	}
      private:  C * ptr_;
      };
    }
    using internal::scoped_ptr;
    enum LogLevel
    {
      LOGLEVEL_INFO, LOGLEVEL_WARNING, LOGLEVEL_ERROR, LOGLEVEL_FATAL,
	LOGLEVEL_DFATAL = LOGLEVEL_ERROR
    };
    namespace internal
    {
      class LogMessage
      {
      public:LogMessage (LogLevel level, const char *filename,
		    int line);
	 ~LogMessage ();
	  LogMessage & operator<< (const std::string & value);
      };
      class LogFinisher
      {
      public:void operator= (LogMessage & other);
      };
    }
    using namespace std;
    class Descriptor
    {
    };
    class FieldDescriptor
    {
    public:
      const Descriptor *message_type () const;
      string DebugString () const;
    };
    class MessageLite
    {
    };
    class Message:public MessageLite
    {
    public:inline Message ()
      {
      }
      virtual ~ Message ();
      virtual Message *New () const = 0;
    };
    class MessageFactory
    {
    };
    class UnknownFieldSet
    {
    };
    class DynamicMessageFactory:public MessageFactory
    {
    public:DynamicMessageFactory ();
      const Message *GetPrototype (const Descriptor * type);
    };
    namespace io
    {
      class ErrorCollector
      {
      public:inline ErrorCollector ()
	{
	}
	virtual ~ ErrorCollector ();
      };
    }
    class DescriptorBuilder
    {
      class OptionInterpreter
      {
	bool SetAggregateOption (const FieldDescriptor * option_field,
				 UnknownFieldSet * unknown_fields);
	DynamicMessageFactory dynamic_factory_;
      };
    };
    namespace
    {
      class AggregateErrorCollector:public io::ErrorCollector
      {
      };
    }
    bool DescriptorBuilder::OptionInterpreter::
      SetAggregateOption (const FieldDescriptor * option_field,
			  UnknownFieldSet * unknown_fields)
    {
      const Descriptor *type = option_field->message_type ();
      scoped_ptr < Message >
	dynamic (dynamic_factory_.GetPrototype (type)->New ());
      !(!(dynamic.get () !=
	  __null)) ? (void) 0 : ::google::protobuf::internal::
	LogFinisher () =::google::protobuf::internal::LogMessage (::google::
								  protobuf::
								  LOGLEVEL_FATAL,
								  "descriptor.cc",
								  4396) <<
	option_field->DebugString ();
      AggregateErrorCollector collector;
    }
  }
}
