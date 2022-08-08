// { dg-options "-w" }
struct PhantomData<T>;

struct Hasher<S> {
    _marker: PhantomData<S>,
}

struct Sip24Rounds;

struct SipHasher24 {
    hasher: Hasher<Sip24Rounds>,
}

impl SipHasher24 {
    pub fn new_with_keys(key0: u64, key1: u64) -> SipHasher24 {
        SipHasher24 {
            hasher: Hasher::new_with_keys(),
        }
    }
}

impl<S> Hasher<S> {
    fn new_with_keys() -> Hasher<S> {
        Hasher {
            _marker: PhantomData,
        }
    }
}
