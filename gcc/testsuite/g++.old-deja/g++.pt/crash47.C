// { dg-do assemble  }
// Origin: Rick Campbell <rick.campbell@db.com>

template <class Owner, typename Type>
struct DataMember
{
    inline DataMember (Type Owner::* data_member);

    Type Owner::* _data_member;
};

template <class Owner, typename Type>
inline
DataMember<Owner,Type>::DataMember (Type Owner::* data_member)
    : _data_member (data_member)
{
}
