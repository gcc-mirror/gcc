// comment line not a doc
/* comment block not a doc                   */

//! inner line comment for most outer crate
/*! inner block comment for most outer crate */

// comment line not a doc
/* comment block not a doc                   */

/// outer doc line for module
/** outer doc block for module               */
pub mod module
{
  //!  inner line doc
  //!! inner line doc!
  /*!  inner block doc  */
  /*!! inner block doc! */

  //   line comment
  ///  outer line doc
  //// line comment

  /*   block comment   */
  /**  outer block doc */
  /*** block comment   */

  mod block_doc_comments
  {
    /*   /* */  /** */  /*! */  */
    /*!  /* */  /** */  /*! */  */
    /**  /* */  /** */  /*! */  */
    mod item { }
  }

  pub mod empty
  {
    //!
    /*!*/
    //

    ///
    mod doc { }

    /**/
    /***/
  }
}
pub fn main () { }
