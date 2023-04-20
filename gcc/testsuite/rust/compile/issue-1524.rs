// { dg-additional-options "-w -frust-compile-until=nameresolution" }
// https://github.com/Rust-GCC/gccrs/issues/1524
// https://github.com/rust-lang/rust/blob/673d0db5e393e9c64897005b470bfeb6d5aec61b/src/test/ui/methods/method-normalize-bounds-issue-20604.rs
trait Hasher {
    type Output;
    fn finish(&self) -> Self::Output;
}

trait Hash<H: Hasher> {
    fn hash(&self, h: &mut H);
}

trait HashState {
    type Wut: Hasher;
    fn hasher(&self) -> Self::Wut;
}

struct SipHasher;
impl Hasher for SipHasher {
    type Output = u64;
    fn finish(&self) -> u64 { 4 }
}

impl Hash<SipHasher> for isize {
    fn hash(&self, h: &mut SipHasher) {}
}

struct SipState;
impl HashState for SipState {
    type Wut = SipHasher;
    fn hasher(&self) -> SipHasher { SipHasher }
}

struct Map<S> {
    s: S,
}

impl<S> Map<S>
    where S: HashState,
          <S as HashState>::Wut: Hasher<Output=u64>,
{
    fn foo<K>(&self, k: K) where K: Hash< <S as HashState>::Wut> {}
}

fn foo<K: Hash<SipHasher>>(map: &Map<SipState>) {
    map.foo(22);
}

fn main() {}
