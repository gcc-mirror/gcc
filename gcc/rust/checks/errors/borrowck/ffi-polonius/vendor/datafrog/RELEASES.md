# 2.0.1

- Work around a rustdoc ICE (#24)

# 2.0.0

- Breaking changes:
  - leapjoin now takes a tuple of leapers, and not a `&mut` slice:
    - `from_leapjoin(&input, &mut [&mut foo.extend_with(...), ..], ..)` becomes
      `from_leapjoin(&input, (foo.extend_with(...), ..), ..)`
    - if there is only one leaper, no tuple is needed
  - `Relation::from` now requires a vector, not an iterator; use
    `Relation::from_iter` instead
- Changed the API to permit using `Relation` and `Variable` more interchangeably,
  and added a number of operations to construct relations directly, like `Relation::from_join`
- Extended leapfrog triejoin with new operations (`PrefixFilter` and `ValueFilter`)

# 1.0.0

- Added leapfrog triejoin (#11).
- Have badges and repo links now!
- Minor performance improvements (#13).

# 0.1.0

- Initial release.
